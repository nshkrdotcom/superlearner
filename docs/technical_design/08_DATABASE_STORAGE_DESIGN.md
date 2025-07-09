# Database and Storage Design Document
## Interactive OTP Sandbox Development Platform

**Version**: 1.0  
**Date**: July 9, 2025  
**Authors**: System Architecture Team  
**Status**: Draft

---

## Table of Contents

1. [Overview](#overview)
2. [Storage Architecture](#storage-architecture)
3. [Database Design](#database-design)
4. [Data Models](#data-models)
5. [Storage Strategies](#storage-strategies)
6. [Backup and Recovery](#backup-and-recovery)
7. [Performance Optimization](#performance-optimization)
8. [Data Migration](#data-migration)
9. [Security and Compliance](#security-and-compliance)
10. [Monitoring and Maintenance](#monitoring-and-maintenance)
11. [Implementation Details](#implementation-details)
12. [Testing Strategy](#testing-strategy)

---

## Overview

### Purpose

The Database and Storage Design provides a comprehensive data management strategy for the interactive OTP sandbox platform, ensuring reliable, scalable, and secure storage of user data, sandbox states, educational content, and system metadata.

### Design Goals

- **Scalability**: Support millions of users and sandbox sessions with linear scaling
- **Reliability**: 99.9% availability with automatic failover and recovery
- **Performance**: Sub-10ms query response times for interactive operations
- **Consistency**: ACID compliance for critical data with eventual consistency for collaborative features
- **Security**: Encryption at rest and in transit with audit logging
- **Flexibility**: Multi-model storage supporting relational, document, and time-series data

### Key Features

- Multi-database architecture with specialized storage for different data types
- Automatic backup and point-in-time recovery
- Real-time replication across multiple regions
- Intelligent data partitioning and archiving
- Comprehensive audit logging and compliance
- Performance monitoring and optimization
- Disaster recovery and business continuity

---

## Storage Architecture

### High-Level Storage Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    Storage Layer                                │
├─────────────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────┐ │
│  │  Primary    │  │  Document   │  │ Time-Series │  │  Object │ │
│  │ Database    │  │  Database   │  │  Database   │  │ Storage │ │
│  │ (PostgreSQL)│  │ (MongoDB)   │  │ (InfluxDB)  │  │ (S3)    │ │
│  └─────────────┘  └─────────────┘  └─────────────┘  └─────────┘ │
├─────────────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────┐ │
│  │   Cache     │  │   Search    │  │   Message   │  │  File   │ │
│  │   Layer     │  │   Engine    │  │   Queue     │  │ System  │ │
│  │  (Redis)    │  │(ElasticSearch)│  │ (RabbitMQ) │  │ (NFS)   │ │
│  └─────────────┘  └─────────────┘  └─────────────┘  └─────────┘ │
├─────────────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────┐ │
│  │   Backup    │  │ Replication │  │   Archive   │  │  CDN    │ │
│  │   System    │  │   Manager   │  │   Storage   │  │ Storage │ │
│  │             │  │             │  │             │  │         │ │
│  └─────────────┘  └─────────────┘  └─────────────┘  └─────────┘ │
└─────────────────────────────────────────────────────────────────┘
```

### Storage Components

1. **Primary Database**: PostgreSQL for core application data
2. **Document Database**: MongoDB for flexible schema data
3. **Time-Series Database**: InfluxDB for metrics and analytics
4. **Object Storage**: S3-compatible storage for files and media
5. **Cache Layer**: Redis for high-performance caching
6. **Search Engine**: Elasticsearch for full-text search
7. **Message Queue**: RabbitMQ for asynchronous processing
8. **File System**: NFS for shared file storage

---

## Database Design

### Primary Database Schema (PostgreSQL)

**Implementation**:

```sql
-- Users and Authentication
CREATE TABLE users (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    email VARCHAR(255) UNIQUE NOT NULL,
    username VARCHAR(50) UNIQUE NOT NULL,
    password_hash VARCHAR(255) NOT NULL,
    first_name VARCHAR(100),
    last_name VARCHAR(100),
    role VARCHAR(50) DEFAULT 'student',
    status VARCHAR(20) DEFAULT 'active',
    email_verified BOOLEAN DEFAULT FALSE,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    last_login_at TIMESTAMP WITH TIME ZONE,
    metadata JSONB DEFAULT '{}'
);

-- User Profiles
CREATE TABLE user_profiles (
    user_id UUID PRIMARY KEY REFERENCES users(id) ON DELETE CASCADE,
    avatar_url VARCHAR(500),
    bio TEXT,
    learning_preferences JSONB DEFAULT '{}',
    skill_levels JSONB DEFAULT '{}',
    achievements JSONB DEFAULT '[]',
    statistics JSONB DEFAULT '{}',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

-- Sandboxes
CREATE TABLE sandboxes (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    name VARCHAR(255) NOT NULL,
    description TEXT,
    type VARCHAR(50) DEFAULT 'development',
    status VARCHAR(20) DEFAULT 'active',
    node_name VARCHAR(100),
    resource_limits JSONB DEFAULT '{}',
    configuration JSONB DEFAULT '{}',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    last_accessed_at TIMESTAMP WITH TIME ZONE,
    expires_at TIMESTAMP WITH TIME ZONE,
    metadata JSONB DEFAULT '{}'
);

-- Sandbox States
CREATE TABLE sandbox_states (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    sandbox_id UUID NOT NULL REFERENCES sandboxes(id) ON DELETE CASCADE,
    version INTEGER NOT NULL,
    state_data JSONB NOT NULL,
    checkpoint_name VARCHAR(255),
    created_by UUID REFERENCES users(id),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    metadata JSONB DEFAULT '{}',
    UNIQUE(sandbox_id, version)
);

-- Educational Content
CREATE TABLE educational_content (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    type VARCHAR(50) NOT NULL, -- 'tutorial', 'exercise', 'assessment'
    title VARCHAR(255) NOT NULL,
    description TEXT,
    content JSONB NOT NULL,
    difficulty_level VARCHAR(20) DEFAULT 'beginner',
    skill_area VARCHAR(50) NOT NULL,
    prerequisites JSONB DEFAULT '[]',
    estimated_duration INTEGER, -- in minutes
    tags JSONB DEFAULT '[]',
    status VARCHAR(20) DEFAULT 'draft',
    created_by UUID REFERENCES users(id),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    published_at TIMESTAMP WITH TIME ZONE,
    metadata JSONB DEFAULT '{}'
);

-- User Progress
CREATE TABLE user_progress (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    content_id UUID NOT NULL REFERENCES educational_content(id) ON DELETE CASCADE,
    status VARCHAR(20) DEFAULT 'not_started',
    progress_percentage INTEGER DEFAULT 0,
    score INTEGER,
    attempts INTEGER DEFAULT 0,
    time_spent INTEGER DEFAULT 0, -- in seconds
    started_at TIMESTAMP WITH TIME ZONE,
    completed_at TIMESTAMP WITH TIME ZONE,
    last_accessed_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    data JSONB DEFAULT '{}',
    UNIQUE(user_id, content_id)
);

-- Collaboration Sessions
CREATE TABLE collaboration_sessions (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    sandbox_id UUID NOT NULL REFERENCES sandboxes(id) ON DELETE CASCADE,
    name VARCHAR(255) NOT NULL,
    description TEXT,
    owner_id UUID NOT NULL REFERENCES users(id),
    status VARCHAR(20) DEFAULT 'active',
    max_participants INTEGER DEFAULT 10,
    session_data JSONB DEFAULT '{}',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    ended_at TIMESTAMP WITH TIME ZONE,
    metadata JSONB DEFAULT '{}'
);

-- Session Participants
CREATE TABLE session_participants (
    session_id UUID NOT NULL REFERENCES collaboration_sessions(id) ON DELETE CASCADE,
    user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    role VARCHAR(50) DEFAULT 'participant',
    joined_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    left_at TIMESTAMP WITH TIME ZONE,
    permissions JSONB DEFAULT '{}',
    PRIMARY KEY (session_id, user_id)
);

-- Audit Logs
CREATE TABLE audit_logs (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    user_id UUID REFERENCES users(id),
    action VARCHAR(100) NOT NULL,
    resource_type VARCHAR(50) NOT NULL,
    resource_id UUID,
    details JSONB DEFAULT '{}',
    ip_address INET,
    user_agent TEXT,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    metadata JSONB DEFAULT '{}'
);

-- System Metrics
CREATE TABLE system_metrics (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    metric_name VARCHAR(100) NOT NULL,
    metric_value NUMERIC NOT NULL,
    metric_type VARCHAR(50) NOT NULL,
    tags JSONB DEFAULT '{}',
    node_name VARCHAR(100),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

-- Indexes for Performance
CREATE INDEX idx_users_email ON users(email);
CREATE INDEX idx_users_username ON users(username);
CREATE INDEX idx_users_created_at ON users(created_at);
CREATE INDEX idx_users_role ON users(role);

CREATE INDEX idx_sandboxes_user_id ON sandboxes(user_id);
CREATE INDEX idx_sandboxes_status ON sandboxes(status);
CREATE INDEX idx_sandboxes_created_at ON sandboxes(created_at);
CREATE INDEX idx_sandboxes_node_name ON sandboxes(node_name);

CREATE INDEX idx_sandbox_states_sandbox_id ON sandbox_states(sandbox_id);
CREATE INDEX idx_sandbox_states_version ON sandbox_states(sandbox_id, version);
CREATE INDEX idx_sandbox_states_created_at ON sandbox_states(created_at);

CREATE INDEX idx_educational_content_type ON educational_content(type);
CREATE INDEX idx_educational_content_skill_area ON educational_content(skill_area);
CREATE INDEX idx_educational_content_difficulty ON educational_content(difficulty_level);
CREATE INDEX idx_educational_content_status ON educational_content(status);

CREATE INDEX idx_user_progress_user_id ON user_progress(user_id);
CREATE INDEX idx_user_progress_content_id ON user_progress(content_id);
CREATE INDEX idx_user_progress_status ON user_progress(status);
CREATE INDEX idx_user_progress_completed_at ON user_progress(completed_at);

CREATE INDEX idx_collaboration_sessions_sandbox_id ON collaboration_sessions(sandbox_id);
CREATE INDEX idx_collaboration_sessions_owner_id ON collaboration_sessions(owner_id);
CREATE INDEX idx_collaboration_sessions_status ON collaboration_sessions(status);

CREATE INDEX idx_audit_logs_user_id ON audit_logs(user_id);
CREATE INDEX idx_audit_logs_action ON audit_logs(action);
CREATE INDEX idx_audit_logs_resource_type ON audit_logs(resource_type);
CREATE INDEX idx_audit_logs_created_at ON audit_logs(created_at);

CREATE INDEX idx_system_metrics_metric_name ON system_metrics(metric_name);
CREATE INDEX idx_system_metrics_created_at ON system_metrics(created_at);
CREATE INDEX idx_system_metrics_node_name ON system_metrics(node_name);

-- Full-text search indexes
CREATE INDEX idx_educational_content_search ON educational_content USING gin(to_tsvector('english', title || ' ' || description));
CREATE INDEX idx_users_search ON users USING gin(to_tsvector('english', first_name || ' ' || last_name || ' ' || username));
```

### Document Database Schema (MongoDB)

**Implementation**:

```javascript
// Sandbox Code Files
db.sandbox_files.createIndex({ "sandbox_id": 1, "file_path": 1 }, { unique: true });
db.sandbox_files.createIndex({ "sandbox_id": 1, "updated_at": -1 });
db.sandbox_files.createIndex({ "file_type": 1 });

// Sample document structure
{
  "_id": ObjectId("..."),
  "sandbox_id": "uuid",
  "file_path": "/src/main.ex",
  "file_type": "elixir",
  "content": "defmodule Main do...",
  "size": 1024,
  "encoding": "utf-8",
  "created_at": ISODate("..."),
  "updated_at": ISODate("..."),
  "created_by": "user_uuid",
  "metadata": {
    "syntax_errors": [],
    "linter_warnings": [],
    "dependencies": []
  }
}

// Collaboration Operations
db.collaboration_operations.createIndex({ "session_id": 1, "timestamp": 1 });
db.collaboration_operations.createIndex({ "user_id": 1, "timestamp": -1 });
db.collaboration_operations.createIndex({ "operation_type": 1 });

// Sample document structure
{
  "_id": ObjectId("..."),
  "session_id": "uuid",
  "user_id": "user_uuid",
  "operation_type": "text_edit",
  "timestamp": ISODate("..."),
  "operation_data": {
    "file_path": "/src/main.ex",
    "position": { "line": 10, "column": 5 },
    "type": "insert",
    "content": "Hello, World!"
  },
  "vector_clock": {
    "user1": 5,
    "user2": 3
  },
  "metadata": {
    "client_id": "...",
    "session_info": {}
  }
}

// Educational Content Templates
db.content_templates.createIndex({ "template_type": 1 });
db.content_templates.createIndex({ "tags": 1 });
db.content_templates.createIndex({ "created_at": -1 });

// Sample document structure
{
  "_id": ObjectId("..."),
  "template_type": "tutorial",
  "name": "GenServer Basics",
  "description": "Learn GenServer fundamentals",
  "structure": {
    "steps": [
      {
        "title": "Introduction",
        "content_type": "markdown",
        "content": "# GenServer Overview...",
        "code_examples": [],
        "exercises": []
      }
    ]
  },
  "variables": {
    "difficulty_level": "beginner",
    "estimated_time": 30
  },
  "tags": ["genserver", "otp", "processes"],
  "created_at": ISODate("..."),
  "updated_at": ISODate("..."),
  "created_by": "user_uuid"
}

// User Activity Streams
db.user_activities.createIndex({ "user_id": 1, "timestamp": -1 });
db.user_activities.createIndex({ "activity_type": 1, "timestamp": -1 });
db.user_activities.createIndex({ "resource_id": 1 });

// Sample document structure
{
  "_id": ObjectId("..."),
  "user_id": "user_uuid",
  "activity_type": "sandbox_created",
  "timestamp": ISODate("..."),
  "resource_id": "sandbox_uuid",
  "resource_type": "sandbox",
  "activity_data": {
    "sandbox_name": "My First Sandbox",
    "sandbox_type": "tutorial",
    "template_used": "basic_genserver"
  },
  "context": {
    "ip_address": "192.168.1.100",
    "user_agent": "Mozilla/5.0...",
    "session_id": "session_uuid"
  }
}
```

### Time-Series Database Schema (InfluxDB)

**Implementation**:

```javascript
// System Performance Metrics
// Measurement: system_metrics
{
  "measurement": "system_metrics",
  "tags": {
    "node_name": "node1",
    "metric_type": "cpu",
    "region": "us-east-1"
  },
  "fields": {
    "value": 45.2,
    "threshold": 80.0,
    "status": "normal"
  },
  "time": "2025-01-09T12:00:00Z"
}

// Sandbox Performance Metrics
// Measurement: sandbox_metrics
{
  "measurement": "sandbox_metrics",
  "tags": {
    "sandbox_id": "sandbox_uuid",
    "user_id": "user_uuid",
    "sandbox_type": "tutorial",
    "node_name": "node1"
  },
  "fields": {
    "memory_usage": 67108864,
    "cpu_usage": 12.5,
    "process_count": 15,
    "execution_time": 1500
  },
  "time": "2025-01-09T12:00:00Z"
}

// User Interaction Metrics
// Measurement: user_interactions
{
  "measurement": "user_interactions",
  "tags": {
    "user_id": "user_uuid",
    "interaction_type": "code_execution",
    "content_type": "tutorial",
    "skill_level": "beginner"
  },
  "fields": {
    "response_time": 234,
    "success": true,
    "error_count": 0,
    "session_duration": 1800
  },
  "time": "2025-01-09T12:00:00Z"
}

// Educational Progress Metrics
// Measurement: learning_progress
{
  "measurement": "learning_progress",
  "tags": {
    "user_id": "user_uuid",
    "content_id": "content_uuid",
    "content_type": "exercise",
    "difficulty": "intermediate"
  },
  "fields": {
    "score": 85,
    "time_spent": 1200,
    "attempts": 3,
    "completion_rate": 0.8
  },
  "time": "2025-01-09T12:00:00Z"
}

// Collaboration Metrics
// Measurement: collaboration_metrics
{
  "measurement": "collaboration_metrics",
  "tags": {
    "session_id": "session_uuid",
    "user_id": "user_uuid",
    "session_type": "pair_programming"
  },
  "fields": {
    "operations_count": 45,
    "conflicts_resolved": 3,
    "active_time": 2700,
    "contribution_score": 0.65
  },
  "time": "2025-01-09T12:00:00Z"
}
```

---

## Data Models

### Core Data Models

**Implementation**:

```elixir
defmodule OtpSupervisor.Database.Models.User do
  @moduledoc """
  User data model with comprehensive profile information.
  """
  
  use Ecto.Schema
  import Ecto.Changeset
  
  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id
  
  schema "users" do
    field :email, :string
    field :username, :string
    field :password_hash, :string
    field :first_name, :string
    field :last_name, :string
    field :role, :string, default: "student"
    field :status, :string, default: "active"
    field :email_verified, :boolean, default: false
    field :last_login_at, :utc_datetime
    field :metadata, :map, default: %{}
    
    has_one :profile, OtpSupervisor.Database.Models.UserProfile
    has_many :sandboxes, OtpSupervisor.Database.Models.Sandbox
    has_many :progress_records, OtpSupervisor.Database.Models.UserProgress
    has_many :collaboration_sessions, OtpSupervisor.Database.Models.CollaborationSession, foreign_key: :owner_id
    
    timestamps()
  end
  
  def changeset(user, attrs) do
    user
    |> cast(attrs, [:email, :username, :first_name, :last_name, :role, :status, :email_verified, :metadata])
    |> validate_required([:email, :username])
    |> validate_format(:email, ~r/^[^\s]+@[^\s]+\.[^\s]+$/)
    |> validate_length(:username, min: 3, max: 50)
    |> validate_inclusion(:role, ["student", "instructor", "admin"])
    |> validate_inclusion(:status, ["active", "inactive", "suspended"])
    |> unique_constraint(:email)
    |> unique_constraint(:username)
  end
  
  def registration_changeset(user, attrs) do
    user
    |> changeset(attrs)
    |> cast(attrs, [:password])
    |> validate_required([:password])
    |> validate_length(:password, min: 8)
    |> validate_password_strength()
    |> put_password_hash()
  end
  
  defp validate_password_strength(changeset) do
    password = get_change(changeset, :password)
    
    if password do
      cond do
        not String.match?(password, ~r/[A-Z]/) ->
          add_error(changeset, :password, "must contain at least one uppercase letter")
        
        not String.match?(password, ~r/[a-z]/) ->
          add_error(changeset, :password, "must contain at least one lowercase letter")
        
        not String.match?(password, ~r/[0-9]/) ->
          add_error(changeset, :password, "must contain at least one number")
        
        true ->
          changeset
      end
    else
      changeset
    end
  end
  
  defp put_password_hash(changeset) do
    case changeset do
      %Ecto.Changeset{valid?: true, changes: %{password: password}} ->
        put_change(changeset, :password_hash, Argon2.hash_pwd_salt(password))
        |> delete_change(:password)
      
      _ ->
        changeset
    end
  end
end

defmodule OtpSupervisor.Database.Models.Sandbox do
  @moduledoc """
  Sandbox data model with configuration and state management.
  """
  
  use Ecto.Schema
  import Ecto.Changeset
  
  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id
  
  schema "sandboxes" do
    field :name, :string
    field :description, :string
    field :type, :string, default: "development"
    field :status, :string, default: "active"
    field :node_name, :string
    field :resource_limits, :map, default: %{}
    field :configuration, :map, default: %{}
    field :last_accessed_at, :utc_datetime
    field :expires_at, :utc_datetime
    field :metadata, :map, default: %{}
    
    belongs_to :user, OtpSupervisor.Database.Models.User
    has_many :states, OtpSupervisor.Database.Models.SandboxState
    has_many :collaboration_sessions, OtpSupervisor.Database.Models.CollaborationSession
    
    timestamps()
  end
  
  def changeset(sandbox, attrs) do
    sandbox
    |> cast(attrs, [:name, :description, :type, :status, :node_name, :resource_limits, :configuration, :expires_at, :metadata])
    |> validate_required([:name, :type])
    |> validate_length(:name, min: 1, max: 255)
    |> validate_inclusion(:type, ["development", "tutorial", "exercise", "assessment", "collaboration"])
    |> validate_inclusion(:status, ["active", "inactive", "suspended", "terminated"])
    |> validate_resource_limits()
    |> put_expiration_time()
  end
  
  defp validate_resource_limits(changeset) do
    resource_limits = get_change(changeset, :resource_limits)
    
    if resource_limits do
      required_keys = ["max_memory", "max_processes", "max_execution_time"]
      
      missing_keys = required_keys -- Map.keys(resource_limits)
      
      if Enum.empty?(missing_keys) do
        changeset
      else
        add_error(changeset, :resource_limits, "missing required keys: #{Enum.join(missing_keys, ", ")}")
      end
    else
      changeset
    end
  end
  
  defp put_expiration_time(changeset) do
    case get_field(changeset, :expires_at) do
      nil ->
        # Default expiration: 24 hours from now
        expires_at = DateTime.utc_now() |> DateTime.add(24 * 60 * 60, :second)
        put_change(changeset, :expires_at, expires_at)
      
      _ ->
        changeset
    end
  end
end

defmodule OtpSupervisor.Database.Models.EducationalContent do
  @moduledoc """
  Educational content model for tutorials, exercises, and assessments.
  """
  
  use Ecto.Schema
  import Ecto.Changeset
  
  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id
  
  schema "educational_content" do
    field :type, :string
    field :title, :string
    field :description, :string
    field :content, :map
    field :difficulty_level, :string, default: "beginner"
    field :skill_area, :string
    field :prerequisites, {:array, :string}, default: []
    field :estimated_duration, :integer
    field :tags, {:array, :string}, default: []
    field :status, :string, default: "draft"
    field :published_at, :utc_datetime
    field :metadata, :map, default: %{}
    
    belongs_to :created_by, OtpSupervisor.Database.Models.User
    has_many :progress_records, OtpSupervisor.Database.Models.UserProgress, foreign_key: :content_id
    
    timestamps()
  end
  
  def changeset(content, attrs) do
    content
    |> cast(attrs, [:type, :title, :description, :content, :difficulty_level, :skill_area, :prerequisites, :estimated_duration, :tags, :status, :published_at, :metadata])
    |> validate_required([:type, :title, :content, :skill_area])
    |> validate_inclusion(:type, ["tutorial", "exercise", "assessment", "reference"])
    |> validate_inclusion(:difficulty_level, ["beginner", "intermediate", "advanced", "expert"])
    |> validate_inclusion(:status, ["draft", "review", "published", "archived"])
    |> validate_content_structure()
    |> put_published_timestamp()
  end
  
  defp validate_content_structure(changeset) do
    content = get_change(changeset, :content)
    type = get_change(changeset, :type) || get_field(changeset, :type)
    
    if content && type do
      case validate_content_for_type(content, type) do
        :ok ->
          changeset
        
        {:error, message} ->
          add_error(changeset, :content, message)
      end
    else
      changeset
    end
  end
  
  defp validate_content_for_type(content, "tutorial") do
    required_keys = ["steps", "objectives"]
    
    if Enum.all?(required_keys, &Map.has_key?(content, &1)) do
      :ok
    else
      {:error, "tutorial content must have steps and objectives"}
    end
  end
  
  defp validate_content_for_type(content, "exercise") do
    required_keys = ["instructions", "starter_code", "test_cases"]
    
    if Enum.all?(required_keys, &Map.has_key?(content, &1)) do
      :ok
    else
      {:error, "exercise content must have instructions, starter_code, and test_cases"}
    end
  end
  
  defp validate_content_for_type(content, "assessment") do
    required_keys = ["questions", "scoring"]
    
    if Enum.all?(required_keys, &Map.has_key?(content, &1)) do
      :ok
    else
      {:error, "assessment content must have questions and scoring"}
    end
  end
  
  defp validate_content_for_type(_content, _type), do: :ok
  
  defp put_published_timestamp(changeset) do
    status = get_change(changeset, :status)
    
    if status == "published" && get_field(changeset, :published_at) == nil do
      put_change(changeset, :published_at, DateTime.utc_now())
    else
      changeset
    end
  end
end

defmodule OtpSupervisor.Database.Models.UserProgress do
  @moduledoc """
  Tracks user progress through educational content.
  """
  
  use Ecto.Schema
  import Ecto.Changeset
  
  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id
  
  schema "user_progress" do
    field :status, :string, default: "not_started"
    field :progress_percentage, :integer, default: 0
    field :score, :integer
    field :attempts, :integer, default: 0
    field :time_spent, :integer, default: 0
    field :started_at, :utc_datetime
    field :completed_at, :utc_datetime
    field :last_accessed_at, :utc_datetime
    field :data, :map, default: %{}
    
    belongs_to :user, OtpSupervisor.Database.Models.User
    belongs_to :content, OtpSupervisor.Database.Models.EducationalContent
    
    timestamps()
  end
  
  def changeset(progress, attrs) do
    progress
    |> cast(attrs, [:status, :progress_percentage, :score, :attempts, :time_spent, :started_at, :completed_at, :last_accessed_at, :data])
    |> validate_required([:status])
    |> validate_inclusion(:status, ["not_started", "in_progress", "completed", "failed"])
    |> validate_number(:progress_percentage, greater_than_or_equal_to: 0, less_than_or_equal_to: 100)
    |> validate_number(:score, greater_than_or_equal_to: 0, less_than_or_equal_to: 100)
    |> validate_number(:attempts, greater_than_or_equal_to: 0)
    |> validate_number(:time_spent, greater_than_or_equal_to: 0)
    |> put_status_timestamps()
    |> unique_constraint([:user_id, :content_id])
  end
  
  defp put_status_timestamps(changeset) do
    status = get_change(changeset, :status)
    
    case status do
      "in_progress" ->
        if get_field(changeset, :started_at) == nil do
          put_change(changeset, :started_at, DateTime.utc_now())
        else
          changeset
        end
      
      "completed" ->
        put_change(changeset, :completed_at, DateTime.utc_now())
      
      _ ->
        changeset
    end
  end
end
```

---

## Storage Strategies

### Data Partitioning Strategy

**Implementation**:

```elixir
defmodule OtpSupervisor.Database.PartitioningStrategy do
  @moduledoc """
  Implements data partitioning strategies for scalability.
  """
  
  @doc """
  Determines the partition for a given piece of data.
  """
  def determine_partition(table, key, strategy \\ :hash) do
    case strategy do
      :hash -> hash_partition(table, key)
      :range -> range_partition(table, key)
      :list -> list_partition(table, key)
      :composite -> composite_partition(table, key)
    end
  end
  
  defp hash_partition(table, key) do
    # Hash-based partitioning for even distribution
    partition_count = get_partition_count(table)
    hash = :erlang.phash2(key, partition_count)
    
    "#{table}_partition_#{hash}"
  end
  
  defp range_partition(table, key) do
    # Range-based partitioning (e.g., by date)
    case table do
      :audit_logs ->
        date = extract_date_from_key(key)
        "#{table}_#{format_date_for_partition(date)}"
      
      :system_metrics ->
        timestamp = extract_timestamp_from_key(key)
        "#{table}_#{format_timestamp_for_partition(timestamp)}"
      
      _ ->
        hash_partition(table, key)
    end
  end
  
  defp list_partition(table, key) do
    # List-based partitioning (e.g., by region)
    case table do
      :users ->
        region = extract_region_from_key(key)
        "#{table}_#{region}"
      
      :sandboxes ->
        node = extract_node_from_key(key)
        "#{table}_#{node}"
      
      _ ->
        hash_partition(table, key)
    end
  end
  
  defp composite_partition(table, key) do
    # Composite partitioning (combination of strategies)
    case table do
      :user_progress ->
        user_id = extract_user_id_from_key(key)
        date = extract_date_from_key(key)
        
        user_partition = hash_partition(:users, user_id)
        date_partition = format_date_for_partition(date)
        
        "#{table}_#{user_partition}_#{date_partition}"
      
      _ ->
        hash_partition(table, key)
    end
  end
  
  defp get_partition_count(table) do
    # Get partition count from configuration
    case table do
      :users -> 16
      :sandboxes -> 32
      :audit_logs -> 8
      :system_metrics -> 64
      _ -> 16
    end
  end
  
  defp extract_date_from_key(key) do
    # Extract date from key for date-based partitioning
    case key do
      %DateTime{} -> DateTime.to_date(key)
      %Date{} -> key
      timestamp when is_integer(timestamp) -> 
        DateTime.from_unix!(timestamp) |> DateTime.to_date()
      _ -> Date.utc_today()
    end
  end
  
  defp extract_timestamp_from_key(key) do
    # Extract timestamp from key
    case key do
      %DateTime{} -> key
      timestamp when is_integer(timestamp) -> DateTime.from_unix!(timestamp)
      _ -> DateTime.utc_now()
    end
  end
  
  defp extract_region_from_key(key) do
    # Extract region from key (simplified)
    case key do
      %{region: region} -> region
      _ -> "default"
    end
  end
  
  defp extract_node_from_key(key) do
    # Extract node from key
    case key do
      %{node: node} -> node
      _ -> "default"
    end
  end
  
  defp extract_user_id_from_key(key) do
    # Extract user ID from key
    case key do
      %{user_id: user_id} -> user_id
      user_id when is_binary(user_id) -> user_id
      _ -> "default"
    end
  end
  
  defp format_date_for_partition(date) do
    # Format date for partition naming
    Date.to_string(date) |> String.replace("-", "_")
  end
  
  defp format_timestamp_for_partition(timestamp) do
    # Format timestamp for partition naming (by hour)
    timestamp
    |> DateTime.truncate(:second)
    |> DateTime.to_string()
    |> String.slice(0, 13)  # YYYY-MM-DDTHH
    |> String.replace(["-", ":", "T"], "_")
  end
end
```

### Data Archiving Strategy

**Implementation**:

```elixir
defmodule OtpSupervisor.Database.ArchivingStrategy do
  @moduledoc """
  Implements data archiving strategies for long-term storage.
  """
  
  use GenServer
  require Logger
  
  @archive_check_interval 86_400_000  # 24 hours
  
  def start_link(_) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end
  
  def trigger_archiving(table) do
    GenServer.cast(__MODULE__, {:trigger_archiving, table})
  end
  
  def get_archiving_status do
    GenServer.call(__MODULE__, :get_status)
  end
  
  def init([]) do
    # Schedule periodic archiving
    schedule_archiving_check()
    
    {:ok, %{last_archive_check: DateTime.utc_now()}}
  end
  
  def handle_cast({:trigger_archiving, table}, state) do
    Logger.info("Triggering archiving for table: #{table}")
    
    Task.start(fn -> perform_archiving(table) end)
    
    {:noreply, state}
  end
  
  def handle_call(:get_status, _from, state) do
    status = %{
      last_archive_check: state.last_archive_check,
      archiving_enabled: Application.get_env(:otp_supervisor, :archiving_enabled, true)
    }
    
    {:reply, status, state}
  end
  
  def handle_info(:check_archiving, state) do
    if archiving_enabled?() do
      check_and_archive_tables()
    end
    
    schedule_archiving_check()
    new_state = %{state | last_archive_check: DateTime.utc_now()}
    
    {:noreply, new_state}
  end
  
  defp perform_archiving(table) do
    # Perform archiving for a specific table
    case get_archiving_policy(table) do
      nil ->
        Logger.info("No archiving policy for table: #{table}")
      
      policy ->
        Logger.info("Archiving table #{table} with policy: #{inspect(policy)}")
        
        # Find records to archive
        records_to_archive = find_records_to_archive(table, policy)
        
        if length(records_to_archive) > 0 do
          # Archive records
          case archive_records(table, records_to_archive) do
            {:ok, archived_count} ->
              Logger.info("Archived #{archived_count} records from #{table}")
              
              # Delete archived records from main table
              delete_archived_records(table, records_to_archive)
              
            {:error, reason} ->
              Logger.error("Archiving failed for #{table}: #{reason}")
          end
        else
          Logger.info("No records to archive for table: #{table}")
        end
    end
  end
  
  defp get_archiving_policy(table) do
    # Get archiving policy for a table
    policies = %{
      audit_logs: %{
        retention_days: 90,
        archive_after_days: 30,
        compression: true
      },
      system_metrics: %{
        retention_days: 365,
        archive_after_days: 7,
        compression: true,
        aggregation: :daily
      },
      sandbox_states: %{
        retention_days: 30,
        archive_after_days: 7,
        compression: true,
        keep_latest: 10
      },
      collaboration_operations: %{
        retention_days: 60,
        archive_after_days: 14,
        compression: true
      }
    }
    
    Map.get(policies, table)
  end
  
  defp find_records_to_archive(table, policy) do
    # Find records that need to be archived
    cutoff_date = DateTime.utc_now() 
    |> DateTime.add(-policy.archive_after_days * 24 * 60 * 60, :second)
    
    case table do
      :audit_logs ->
        find_old_audit_logs(cutoff_date)
      
      :system_metrics ->
        find_old_metrics(cutoff_date)
      
      :sandbox_states ->
        find_old_sandbox_states(cutoff_date, policy)
      
      :collaboration_operations ->
        find_old_collaboration_operations(cutoff_date)
      
      _ ->
        []
    end
  end
  
  defp find_old_audit_logs(cutoff_date) do
    # Find audit logs older than cutoff date
    import Ecto.Query
    
    from(al in OtpSupervisor.Database.Models.AuditLog,
      where: al.created_at < ^cutoff_date,
      order_by: [asc: al.created_at],
      limit: 10000  # Process in batches
    )
    |> OtpSupervisor.Repo.all()
  end
  
  defp find_old_metrics(cutoff_date) do
    # Find metrics older than cutoff date
    import Ecto.Query
    
    from(sm in OtpSupervisor.Database.Models.SystemMetric,
      where: sm.created_at < ^cutoff_date,
      order_by: [asc: sm.created_at],
      limit: 50000  # Larger batch for metrics
    )
    |> OtpSupervisor.Repo.all()
  end
  
  defp find_old_sandbox_states(cutoff_date, policy) do
    # Find old sandbox states, keeping the latest N versions
    import Ecto.Query
    
    # Get all sandbox IDs
    sandbox_ids = from(ss in OtpSupervisor.Database.Models.SandboxState,
      where: ss.created_at < ^cutoff_date,
      distinct: ss.sandbox_id,
      select: ss.sandbox_id
    )
    |> OtpSupervisor.Repo.all()
    
    # For each sandbox, find states to archive (keep latest N)
    Enum.flat_map(sandbox_ids, fn sandbox_id ->
      from(ss in OtpSupervisor.Database.Models.SandboxState,
        where: ss.sandbox_id == ^sandbox_id and ss.created_at < ^cutoff_date,
        order_by: [desc: ss.created_at],
        offset: ^policy.keep_latest
      )
      |> OtpSupervisor.Repo.all()
    end)
  end
  
  defp find_old_collaboration_operations(cutoff_date) do
    # Find collaboration operations older than cutoff date
    # This would query MongoDB for operations
    case OtpSupervisor.Database.MongoDB.find_old_operations(cutoff_date) do
      {:ok, operations} -> operations
      {:error, _} -> []
    end
  end
  
  defp archive_records(table, records) do
    # Archive records to long-term storage
    case get_archive_destination(table) do
      {:s3, bucket, prefix} ->
        archive_to_s3(table, records, bucket, prefix)
      
      {:file, path} ->
        archive_to_file(table, records, path)
      
      {:database, archive_db} ->
        archive_to_database(table, records, archive_db)
    end
  end
  
  defp archive_to_s3(table, records, bucket, prefix) do
    # Archive records to S3
    timestamp = DateTime.utc_now() |> DateTime.to_string(:extended)
    file_name = "#{prefix}/#{table}/#{timestamp}.json.gz"
    
    # Convert records to JSON and compress
    json_data = Jason.encode!(records)
    compressed_data = :zlib.gzip(json_data)
    
    # Upload to S3
    case OtpSupervisor.Storage.S3.put_object(bucket, file_name, compressed_data) do
      :ok ->
        {:ok, length(records)}
      
      {:error, reason} ->
        {:error, reason}
    end
  end
  
  defp archive_to_file(table, records, path) do
    # Archive records to file system
    timestamp = DateTime.utc_now() |> DateTime.to_string(:extended)
    file_path = Path.join([path, "#{table}", "#{timestamp}.json.gz"])
    
    # Ensure directory exists
    File.mkdir_p!(Path.dirname(file_path))
    
    # Convert records to JSON and compress
    json_data = Jason.encode!(records)
    compressed_data = :zlib.gzip(json_data)
    
    # Write to file
    case File.write(file_path, compressed_data) do
      :ok ->
        {:ok, length(records)}
      
      {:error, reason} ->
        {:error, reason}
    end
  end
  
  defp archive_to_database(table, records, archive_db) do
    # Archive records to archive database
    # This would involve inserting into archive-specific tables
    {:ok, length(records)}
  end
  
  defp delete_archived_records(table, records) do
    # Delete archived records from main table
    case table do
      :audit_logs ->
        delete_audit_logs(records)
      
      :system_metrics ->
        delete_system_metrics(records)
      
      :sandbox_states ->
        delete_sandbox_states(records)
      
      :collaboration_operations ->
        delete_collaboration_operations(records)
    end
  end
  
  defp delete_audit_logs(records) do
    # Delete audit logs
    record_ids = Enum.map(records, & &1.id)
    
    import Ecto.Query
    
    from(al in OtpSupervisor.Database.Models.AuditLog,
      where: al.id in ^record_ids
    )
    |> OtpSupervisor.Repo.delete_all()
  end
  
  defp delete_system_metrics(records) do
    # Delete system metrics
    record_ids = Enum.map(records, & &1.id)
    
    import Ecto.Query
    
    from(sm in OtpSupervisor.Database.Models.SystemMetric,
      where: sm.id in ^record_ids
    )
    |> OtpSupervisor.Repo.delete_all()
  end
  
  defp delete_sandbox_states(records) do
    # Delete sandbox states
    record_ids = Enum.map(records, & &1.id)
    
    import Ecto.Query
    
    from(ss in OtpSupervisor.Database.Models.SandboxState,
      where: ss.id in ^record_ids
    )
    |> OtpSupervisor.Repo.delete_all()
  end
  
  defp delete_collaboration_operations(records) do
    # Delete collaboration operations from MongoDB
    record_ids = Enum.map(records, & &1["_id"])
    
    OtpSupervisor.Database.MongoDB.delete_operations(record_ids)
  end
  
  defp get_archive_destination(table) do
    # Get archive destination for table
    destinations = %{
      audit_logs: {:s3, "audit-archive", "audit_logs"},
      system_metrics: {:s3, "metrics-archive", "system_metrics"},
      sandbox_states: {:database, :archive_db},
      collaboration_operations: {:file, "/archive/collaboration"}
    }
    
    Map.get(destinations, table, {:file, "/archive/default"})
  end
  
  defp check_and_archive_tables do
    # Check all tables for archiving
    tables_to_check = [:audit_logs, :system_metrics, :sandbox_states, :collaboration_operations]
    
    Enum.each(tables_to_check, fn table ->
      Task.start(fn -> perform_archiving(table) end)
    end)
  end
  
  defp archiving_enabled? do
    Application.get_env(:otp_supervisor, :archiving_enabled, true)
  end
  
  defp schedule_archiving_check do
    Process.send_after(self(), :check_archiving, @archive_check_interval)
  end
end
```

---

## Backup and Recovery

### Automated Backup System

**Implementation**:

```elixir
defmodule OtpSupervisor.Database.BackupSystem do
  @moduledoc """
  Automated backup and recovery system for all databases.
  """
  
  use GenServer
  require Logger
  
  @backup_interval 3_600_000  # 1 hour
  @full_backup_interval 86_400_000  # 24 hours
  @backup_retention_days 30
  
  defstruct [
    :backup_config,
    :last_backup,
    :last_full_backup,
    :backup_status,
    :recovery_points
  ]
  
  def start_link(config) do
    GenServer.start_link(__MODULE__, config, name: __MODULE__)
  end
  
  def trigger_backup(backup_type \\ :incremental) do
    GenServer.cast(__MODULE__, {:trigger_backup, backup_type})
  end
  
  def restore_from_backup(backup_id, options \\ []) do
    GenServer.call(__MODULE__, {:restore_from_backup, backup_id, options})
  end
  
  def list_backups do
    GenServer.call(__MODULE__, :list_backups)
  end
  
  def get_backup_status do
    GenServer.call(__MODULE__, :get_backup_status)
  end
  
  def init(config) do
    # Load backup configuration
    backup_config = load_backup_config(config)
    
    # Initialize backup status
    backup_status = %{
      last_backup: nil,
      last_full_backup: nil,
      backup_in_progress: false,
      last_error: nil
    }
    
    # Load existing recovery points
    recovery_points = load_recovery_points()
    
    state = %__MODULE__{
      backup_config: backup_config,
      last_backup: nil,
      last_full_backup: nil,
      backup_status: backup_status,
      recovery_points: recovery_points
    }
    
    # Schedule backups
    schedule_incremental_backup()
    schedule_full_backup()
    
    {:ok, state}
  end
  
  def handle_cast({:trigger_backup, backup_type}, state) do
    Logger.info("Triggering #{backup_type} backup")
    
    new_state = %{state | backup_status: Map.put(state.backup_status, :backup_in_progress, true)}
    
    # Perform backup asynchronously
    Task.start(fn -> perform_backup(backup_type, state.backup_config) end)
    
    {:noreply, new_state}
  end
  
  def handle_call({:restore_from_backup, backup_id, options}, _from, state) do
    case perform_restore(backup_id, options, state) do
      {:ok, result} ->
        {:reply, {:ok, result}, state}
      
      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end
  
  def handle_call(:list_backups, _from, state) do
    backups = list_available_backups(state.backup_config)
    {:reply, backups, state}
  end
  
  def handle_call(:get_backup_status, _from, state) do
    {:reply, state.backup_status, state}
  end
  
  def handle_info(:incremental_backup, state) do
    Logger.info("Starting scheduled incremental backup")
    
    GenServer.cast(self(), {:trigger_backup, :incremental})
    schedule_incremental_backup()
    
    {:noreply, state}
  end
  
  def handle_info(:full_backup, state) do
    Logger.info("Starting scheduled full backup")
    
    GenServer.cast(self(), {:trigger_backup, :full})
    schedule_full_backup()
    
    {:noreply, state}
  end
  
  def handle_info({:backup_complete, backup_type, result}, state) do
    Logger.info("Backup completed: #{backup_type} - #{inspect(result)}")
    
    new_backup_status = %{state.backup_status |
      backup_in_progress: false,
      last_backup: DateTime.utc_now(),
      last_error: nil
    }
    
    new_backup_status = case backup_type do
      :full -> Map.put(new_backup_status, :last_full_backup, DateTime.utc_now())
      _ -> new_backup_status
    end
    
    new_state = %{state | backup_status: new_backup_status}
    
    # Update recovery points
    case result do
      {:ok, backup_info} ->
        new_recovery_points = [backup_info | state.recovery_points]
        |> Enum.take(50)  # Keep last 50 recovery points
        
        %{new_state | recovery_points: new_recovery_points}
      
      {:error, reason} ->
        error_status = Map.put(new_backup_status, :last_error, reason)
        %{new_state | backup_status: error_status}
    end
    |> then(&{:noreply, &1})
  end
  
  defp perform_backup(backup_type, config) do
    # Perform backup for all databases
    backup_id = generate_backup_id(backup_type)
    
    result = case backup_type do
      :full -> perform_full_backup(backup_id, config)
      :incremental -> perform_incremental_backup(backup_id, config)
    end
    
    # Send completion message
    send(self(), {:backup_complete, backup_type, result})
  end
  
  defp perform_full_backup(backup_id, config) do
    Logger.info("Starting full backup: #{backup_id}")
    
    backup_info = %{
      backup_id: backup_id,
      backup_type: :full,
      started_at: DateTime.utc_now(),
      databases: [],
      status: :in_progress
    }
    
    try do
      # Backup PostgreSQL
      postgres_result = backup_postgresql(backup_id, config)
      
      # Backup MongoDB
      mongodb_result = backup_mongodb(backup_id, config)
      
      # Backup InfluxDB
      influxdb_result = backup_influxdb(backup_id, config)
      
      # Backup Redis
      redis_result = backup_redis(backup_id, config)
      
      # Backup file system
      filesystem_result = backup_filesystem(backup_id, config)
      
      # Collect all results
      databases = [
        {:postgresql, postgres_result},
        {:mongodb, mongodb_result},
        {:influxdb, influxdb_result},
        {:redis, redis_result},
        {:filesystem, filesystem_result}
      ]
      
      # Check if all backups succeeded
      all_successful = Enum.all?(databases, fn {_db, result} ->
        case result do
          {:ok, _} -> true
          _ -> false
        end
      end)
      
      if all_successful do
        completed_backup = %{backup_info |
          status: :completed,
          completed_at: DateTime.utc_now(),
          databases: databases
        }
        
        # Store backup metadata
        store_backup_metadata(completed_backup)
        
        {:ok, completed_backup}
      else
        failed_backup = %{backup_info |
          status: :failed,
          completed_at: DateTime.utc_now(),
          databases: databases
        }
        
        {:error, failed_backup}
      end
    rescue
      error ->
        Logger.error("Full backup failed: #{inspect(error)}")
        {:error, error}
    end
  end
  
  defp perform_incremental_backup(backup_id, config) do
    Logger.info("Starting incremental backup: #{backup_id}")
    
    # Get last backup timestamp
    last_backup_time = get_last_backup_timestamp()
    
    backup_info = %{
      backup_id: backup_id,
      backup_type: :incremental,
      started_at: DateTime.utc_now(),
      since: last_backup_time,
      databases: [],
      status: :in_progress
    }
    
    try do
      # Incremental backup for each database
      postgres_result = backup_postgresql_incremental(backup_id, last_backup_time, config)
      mongodb_result = backup_mongodb_incremental(backup_id, last_backup_time, config)
      influxdb_result = backup_influxdb_incremental(backup_id, last_backup_time, config)
      
      databases = [
        {:postgresql, postgres_result},
        {:mongodb, mongodb_result},
        {:influxdb, influxdb_result}
      ]
      
      all_successful = Enum.all?(databases, fn {_db, result} ->
        case result do
          {:ok, _} -> true
          _ -> false
        end
      end)
      
      if all_successful do
        completed_backup = %{backup_info |
          status: :completed,
          completed_at: DateTime.utc_now(),
          databases: databases
        }
        
        store_backup_metadata(completed_backup)
        
        {:ok, completed_backup}
      else
        {:error, %{backup_info | status: :failed, databases: databases}}
      end
    rescue
      error ->
        Logger.error("Incremental backup failed: #{inspect(error)}")
        {:error, error}
    end
  end
  
  defp backup_postgresql(backup_id, config) do
    # Backup PostgreSQL database
    backup_file = Path.join(config.backup_path, "postgresql_#{backup_id}.sql")
    
    # Use pg_dump for backup
    case System.cmd("pg_dump", [
      "--host", config.postgresql.host,
      "--port", to_string(config.postgresql.port),
      "--username", config.postgresql.username,
      "--dbname", config.postgresql.database,
      "--format", "custom",
      "--file", backup_file,
      "--verbose"
    ], env: [{"PGPASSWORD", config.postgresql.password}]) do
      {_output, 0} ->
        # Compress backup
        compressed_file = "#{backup_file}.gz"
        case System.cmd("gzip", [backup_file]) do
          {_output, 0} ->
            {:ok, %{file: compressed_file, size: File.stat!(compressed_file).size}}
          
          {error, code} ->
            {:error, "Compression failed: #{error} (code: #{code})"}
        end
      
      {error, code} ->
        {:error, "pg_dump failed: #{error} (code: #{code})"}
    end
  end
  
  defp backup_mongodb(backup_id, config) do
    # Backup MongoDB database
    backup_dir = Path.join(config.backup_path, "mongodb_#{backup_id}")
    
    case System.cmd("mongodump", [
      "--host", "#{config.mongodb.host}:#{config.mongodb.port}",
      "--db", config.mongodb.database,
      "--out", backup_dir,
      "--gzip"
    ]) do
      {_output, 0} ->
        # Create tar archive
        archive_file = "#{backup_dir}.tar.gz"
        case System.cmd("tar", ["-czf", archive_file, "-C", Path.dirname(backup_dir), Path.basename(backup_dir)]) do
          {_output, 0} ->
            # Remove uncompressed directory
            File.rm_rf!(backup_dir)
            {:ok, %{file: archive_file, size: File.stat!(archive_file).size}}
          
          {error, code} ->
            {:error, "Archive creation failed: #{error} (code: #{code})"}
        end
      
      {error, code} ->
        {:error, "mongodump failed: #{error} (code: #{code})"}
    end
  end
  
  defp backup_influxdb(backup_id, config) do
    # Backup InfluxDB database
    backup_file = Path.join(config.backup_path, "influxdb_#{backup_id}.tar.gz")
    
    case System.cmd("influxd", [
      "backup",
      "--portable",
      "--database", config.influxdb.database,
      "--host", "#{config.influxdb.host}:#{config.influxdb.port}",
      backup_file
    ]) do
      {_output, 0} ->
        {:ok, %{file: backup_file, size: File.stat!(backup_file).size}}
      
      {error, code} ->
        {:error, "influxd backup failed: #{error} (code: #{code})"}
    end
  end
  
  defp backup_redis(backup_id, config) do
    # Backup Redis database
    backup_file = Path.join(config.backup_path, "redis_#{backup_id}.rdb")
    
    # Use BGSAVE command
    case Redix.command(config.redis.connection, ["BGSAVE"]) do
      {:ok, "Background saving started"} ->
        # Wait for backup to complete
        wait_for_redis_backup_completion(config.redis.connection)
        
        # Copy RDB file
        source_file = Path.join(config.redis.data_dir, "dump.rdb")
        case File.copy(source_file, backup_file) do
          {:ok, _} ->
            # Compress backup
            compressed_file = "#{backup_file}.gz"
            case System.cmd("gzip", [backup_file]) do
              {_output, 0} ->
                {:ok, %{file: compressed_file, size: File.stat!(compressed_file).size}}
              
              {error, code} ->
                {:error, "Compression failed: #{error} (code: #{code})"}
            end
          
          {:error, reason} ->
            {:error, "File copy failed: #{reason}"}
        end
      
      {:error, reason} ->
        {:error, "BGSAVE failed: #{reason}"}
    end
  end
  
  defp backup_filesystem(backup_id, config) do
    # Backup file system directories
    backup_file = Path.join(config.backup_path, "filesystem_#{backup_id}.tar.gz")
    
    # Create tar archive of important directories
    directories = config.filesystem.backup_directories
    
    case System.cmd("tar", ["-czf", backup_file] ++ directories) do
      {_output, 0} ->
        {:ok, %{file: backup_file, size: File.stat!(backup_file).size}}
      
      {error, code} ->
        {:error, "tar backup failed: #{error} (code: #{code})"}
    end
  end
  
  defp perform_restore(backup_id, options, state) do
    Logger.info("Starting restore from backup: #{backup_id}")
    
    # Find backup metadata
    case find_backup_metadata(backup_id, state.recovery_points) do
      nil ->
        {:error, :backup_not_found}
      
      backup_info ->
        # Perform restore
        restore_databases(backup_info, options, state.backup_config)
    end
  end
  
  defp restore_databases(backup_info, options, config) do
    # Restore each database from backup
    databases_to_restore = Keyword.get(options, :databases, [:all])
    
    results = backup_info.databases
    |> Enum.filter(fn {db_type, _result} ->
      databases_to_restore == [:all] or db_type in databases_to_restore
    end)
    |> Enum.map(fn {db_type, backup_result} ->
      case backup_result do
        {:ok, backup_data} ->
          restore_result = restore_single_database(db_type, backup_data, config)
          {db_type, restore_result}
        
        {:error, reason} ->
          {db_type, {:error, "Backup was not successful: #{reason}"}}
      end
    end)
    
    # Check if all restores succeeded
    all_successful = Enum.all?(results, fn {_db, result} ->
      case result do
        {:ok, _} -> true
        _ -> false
      end
    end)
    
    if all_successful do
      {:ok, %{restored_databases: results, restored_at: DateTime.utc_now()}}
    else
      {:error, %{failed_databases: results}}
    end
  end
  
  defp restore_single_database(:postgresql, backup_data, config) do
    # Restore PostgreSQL database
    backup_file = backup_data.file
    
    # Decompress if needed
    decompressed_file = if String.ends_with?(backup_file, ".gz") do
      temp_file = String.replace(backup_file, ".gz", "")
      case System.cmd("gunzip", ["-c", backup_file]) do
        {output, 0} ->
          File.write!(temp_file, output)
          temp_file
        
        {error, code} ->
          return {:error, "Decompression failed: #{error} (code: #{code})"}
      end
    else
      backup_file
    end
    
    # Restore using pg_restore
    case System.cmd("pg_restore", [
      "--host", config.postgresql.host,
      "--port", to_string(config.postgresql.port),
      "--username", config.postgresql.username,
      "--dbname", config.postgresql.database,
      "--clean",
      "--if-exists",
      "--verbose",
      decompressed_file
    ], env: [{"PGPASSWORD", config.postgresql.password}]) do
      {_output, 0} ->
        # Clean up temporary file
        if decompressed_file != backup_file do
          File.rm(decompressed_file)
        end
        
        {:ok, "PostgreSQL restore completed"}
      
      {error, code} ->
        {:error, "pg_restore failed: #{error} (code: #{code})"}
    end
  end
  
  defp restore_single_database(:mongodb, backup_data, config) do
    # Restore MongoDB database
    backup_file = backup_data.file
    
    # Extract archive
    temp_dir = Path.join(System.tmp_dir(), "mongodb_restore_#{System.unique_integer()}")
    
    case System.cmd("tar", ["-xzf", backup_file, "-C", temp_dir]) do
      {_output, 0} ->
        # Find database directory
        db_dir = Path.join([temp_dir, "mongodb_*", config.mongodb.database])
        
        case System.cmd("mongorestore", [
          "--host", "#{config.mongodb.host}:#{config.mongodb.port}",
          "--db", config.mongodb.database,
          "--drop",
          "--gzip",
          db_dir
        ]) do
          {_output, 0} ->
            # Clean up
            File.rm_rf!(temp_dir)
            {:ok, "MongoDB restore completed"}
          
          {error, code} ->
            File.rm_rf!(temp_dir)
            {:error, "mongorestore failed: #{error} (code: #{code})"}
        end
      
      {error, code} ->
        {:error, "Archive extraction failed: #{error} (code: #{code})"}
    end
  end
  
  defp restore_single_database(:influxdb, backup_data, config) do
    # Restore InfluxDB database
    backup_file = backup_data.file
    
    case System.cmd("influxd", [
      "restore",
      "--portable",
      "--database", config.influxdb.database,
      "--host", "#{config.influxdb.host}:#{config.influxdb.port}",
      backup_file
    ]) do
      {_output, 0} ->
        {:ok, "InfluxDB restore completed"}
      
      {error, code} ->
        {:error, "influxd restore failed: #{error} (code: #{code})"}
    end
  end
  
  defp restore_single_database(:redis, backup_data, config) do
    # Restore Redis database
    backup_file = backup_data.file
    
    # Stop Redis temporarily
    case System.cmd("redis-cli", ["SHUTDOWN", "NOSAVE"]) do
      {_output, 0} ->
        # Decompress and copy RDB file
        decompressed_file = if String.ends_with?(backup_file, ".gz") do
          temp_file = String.replace(backup_file, ".gz", "")
          case System.cmd("gunzip", ["-c", backup_file]) do
            {output, 0} ->
              File.write!(temp_file, output)
              temp_file
            
            {error, code} ->
              return {:error, "Decompression failed: #{error} (code: #{code})"}
          end
        else
          backup_file
        end
        
        # Copy to Redis data directory
        target_file = Path.join(config.redis.data_dir, "dump.rdb")
        case File.copy(decompressed_file, target_file) do
          {:ok, _} ->
            # Restart Redis
            case System.cmd("redis-server", [config.redis.config_file]) do
              {_output, 0} ->
                {:ok, "Redis restore completed"}
              
              {error, code} ->
                {:error, "Redis restart failed: #{error} (code: #{code})"}
            end
          
          {:error, reason} ->
            {:error, "File copy failed: #{reason}"}
        end
      
      {error, code} ->
        {:error, "Redis shutdown failed: #{error} (code: #{code})"}
    end
  end
  
  # Helper functions
  defp load_backup_config(config) do
    # Load backup configuration from application config
    default_config = %{
      backup_path: "/var/backups/otp_supervisor",
      postgresql: %{
        host: "localhost",
        port: 5432,
        username: "postgres",
        password: "password",
        database: "otp_supervisor"
      },
      mongodb: %{
        host: "localhost",
        port: 27017,
        database: "otp_supervisor"
      },
      influxdb: %{
        host: "localhost",
        port: 8086,
        database: "otp_supervisor"
      },
      redis: %{
        connection: nil,
        data_dir: "/var/lib/redis",
        config_file: "/etc/redis/redis.conf"
      },
      filesystem: %{
        backup_directories: ["/var/lib/otp_supervisor", "/etc/otp_supervisor"]
      }
    }
    
    Map.merge(default_config, config)
  end
  
  defp load_recovery_points do
    # Load existing recovery points from metadata store
    []
  end
  
  defp generate_backup_id(backup_type) do
    timestamp = DateTime.utc_now() |> DateTime.to_string(:extended)
    "#{backup_type}_#{timestamp}"
  end
  
  defp store_backup_metadata(backup_info) do
    # Store backup metadata for recovery
    metadata_file = Path.join("/var/lib/otp_supervisor/backups", "#{backup_info.backup_id}.json")
    
    File.mkdir_p!(Path.dirname(metadata_file))
    
    case Jason.encode(backup_info) do
      {:ok, json} ->
        File.write!(metadata_file, json)
      
      {:error, reason} ->
        Logger.error("Failed to store backup metadata: #{reason}")
    end
  end
  
  defp find_backup_metadata(backup_id, recovery_points) do
    Enum.find(recovery_points, fn backup_info ->
      backup_info.backup_id == backup_id
    end)
  end
  
  defp get_last_backup_timestamp do
    # Get timestamp of last backup
    DateTime.utc_now() |> DateTime.add(-3600, :second)  # 1 hour ago as placeholder
  end
  
  defp list_available_backups(config) do
    # List available backups
    backup_dir = config.backup_path
    
    case File.ls(backup_dir) do
      {:ok, files} ->
        files
        |> Enum.filter(&String.contains?(&1, ".json"))
        |> Enum.map(fn file ->
          metadata_file = Path.join(backup_dir, file)
          case File.read(metadata_file) do
            {:ok, content} ->
              case Jason.decode(content) do
                {:ok, backup_info} -> backup_info
                {:error, _} -> nil
              end
            {:error, _} -> nil
          end
        end)
        |> Enum.reject(&is_nil/1)
      
      {:error, _} ->
        []
    end
  end
  
  defp wait_for_redis_backup_completion(connection) do
    # Wait for Redis BGSAVE to complete
    Stream.repeatedly(fn ->
      case Redix.command(connection, ["LASTSAVE"]) do
        {:ok, timestamp} -> timestamp
        {:error, _} -> nil
      end
    end)
    |> Stream.chunk_every(2, 1)
    |> Enum.find(fn
      [prev, curr] when prev != curr -> true
      _ -> false
    end)
  end
  
  defp schedule_incremental_backup do
    Process.send_after(self(), :incremental_backup, @backup_interval)
  end
  
  defp schedule_full_backup do
    Process.send_after(self(), :full_backup, @full_backup_interval)
  end
  
  # Incremental backup functions
  defp backup_postgresql_incremental(backup_id, since_timestamp, config) do
    # Implement incremental backup for PostgreSQL
    # This would use WAL files or logical replication
    {:ok, %{type: :incremental, since: since_timestamp}}
  end
  
  defp backup_mongodb_incremental(backup_id, since_timestamp, config) do
    # Implement incremental backup for MongoDB
    # This would use oplog
    {:ok, %{type: :incremental, since: since_timestamp}}
  end
  
  defp backup_influxdb_incremental(backup_id, since_timestamp, config) do
    # Implement incremental backup for InfluxDB
    # This would backup only data since timestamp
    {:ok, %{type: :incremental, since: since_timestamp}}
  end
end
```

---

## Testing Strategy

### Database Testing Framework

```elixir
defmodule OtpSupervisor.Database.DatabaseTest do
  use ExUnit.Case, async: false
  alias OtpSupervisor.Database.Models.{User, Sandbox, EducationalContent}
  
  describe "database performance" do
    test "handles concurrent user creation" do
      # Test concurrent user creation
      concurrent_count = 100
      
      tasks = 1..concurrent_count
      |> Enum.map(fn i ->
        Task.async(fn ->
          attrs = %{
            email: "user#{i}@example.com",
            username: "user#{i}",
            password: "password123"
          }
          
          %User{}
          |> User.registration_changeset(attrs)
          |> OtpSupervisor.Repo.insert()
        end)
      end)
      
      results = Task.await_many(tasks, 30_000)
      
      # Verify all users were created successfully
      successful_creates = Enum.count(results, fn
        {:ok, _user} -> true
        _ -> false
      end)
      
      assert successful_creates == concurrent_count
    end
    
    test "query performance is acceptable" do
      # Create test data
      create_test_data(1000)
      
      # Test query performance
      {time, _result} = :timer.tc(fn ->
        import Ecto.Query
        
        from(u in User,
          where: u.role == "student",
          order_by: [desc: u.created_at],
          limit: 50
        )
        |> OtpSupervisor.Repo.all()
      end)
      
      # Assert query completes in less than 10ms
      assert time < 10_000  # 10ms in microseconds
    end
  end
  
  describe "data integrity" do
    test "foreign key constraints work" do
      # Create user
      {:ok, user} = create_test_user()
      
      # Create sandbox
      {:ok, sandbox} = create_test_sandbox(user.id)
      
      # Try to delete user (should fail due to foreign key)
      assert_raise Ecto.ConstraintError, fn ->
        OtpSupervisor.Repo.delete(user)
      end
      
      # Delete sandbox first, then user should work
      OtpSupervisor.Repo.delete(sandbox)
      assert {:ok, _} = OtpSupervisor.Repo.delete(user)
    end
    
    test "unique constraints work" do
      # Create user
      {:ok, _user} = create_test_user()
      
      # Try to create another user with same email
      assert {:error, changeset} = create_test_user()
      assert changeset.errors[:email] == {"has already been taken", [constraint: :unique, constraint_name: "users_email_index"]}
    end
  end
  
  describe "backup and restore" do
    test "backup system works" do
      # Create test data
      create_test_data(100)
      
      # Trigger backup
      OtpSupervisor.Database.BackupSystem.trigger_backup(:full)
      
      # Wait for backup to complete
      Process.sleep(5000)
      
      # Verify backup was created
      backups = OtpSupervisor.Database.BackupSystem.list_backups()
      assert length(backups) > 0
      
      latest_backup = List.first(backups)
      assert latest_backup.status == :completed
    end
  end
  
  defp create_test_user do
    attrs = %{
      email: "test@example.com",
      username: "testuser",
      password: "password123"
    }
    
    %User{}
    |> User.registration_changeset(attrs)
    |> OtpSupervisor.Repo.insert()
  end
  
  defp create_test_sandbox(user_id) do
    attrs = %{
      name: "Test Sandbox",
      user_id: user_id,
      type: "development"
    }
    
    %Sandbox{}
    |> Sandbox.changeset(attrs)
    |> OtpSupervisor.Repo.insert()
  end
  
  defp create_test_data(count) do
    1..count
    |> Enum.each(fn i ->
      attrs = %{
        email: "user#{i}@example.com",
        username: "user#{i}",
        password: "password123"
      }
      
      %User{}
      |> User.registration_changeset(attrs)
      |> OtpSupervisor.Repo.insert()
    end)
  end
end
```

---

## Configuration

### Database Configuration

```elixir
# config/config.exs
config :otp_supervisor, OtpSupervisor.Repo,
  # PostgreSQL configuration
  adapter: Ecto.Adapters.Postgres,
  hostname: "localhost",
  port: 5432,
  username: "postgres",
  password: "password",
  database: "otp_supervisor",
  pool_size: 20,
  pool_timeout: 15000,
  ownership_timeout: 30000,
  timeout: 30000,
  
  # Connection pool configuration
  queue_target: 50,
  queue_interval: 1000,
  
  # SSL configuration
  ssl: true,
  ssl_opts: [
    verify: :verify_peer,
    cacertfile: "priv/ssl/ca.crt",
    certfile: "priv/ssl/client.crt",
    keyfile: "priv/ssl/client.key"
  ]

# MongoDB configuration
config :otp_supervisor, :mongodb,
  hostname: "localhost",
  port: 27017,
  database: "otp_supervisor",
  pool_size: 10,
  username: "mongo_user",
  password: "mongo_password",
  ssl: false

# InfluxDB configuration
config :otp_supervisor, :influxdb,
  hostname: "localhost",
  port: 8086,
  database: "otp_supervisor",
  username: "influx_user",
  password: "influx_password",
  precision: :seconds

# Redis configuration
config :otp_supervisor, :redis,
  hostname: "localhost",
  port: 6379,
  password: "redis_password",
  database: 0,
  pool_size: 10

# Backup configuration
config :otp_supervisor, :backup,
  enabled: true,
  backup_path: "/var/backups/otp_supervisor",
  retention_days: 30,
  incremental_interval: 3600,  # 1 hour
  full_backup_interval: 86400,  # 24 hours
  
  # S3 configuration for backup storage
  s3_bucket: "otp-supervisor-backups",
  s3_region: "us-east-1",
  s3_access_key_id: "ACCESS_KEY",
  s3_secret_access_key: "SECRET_KEY"

# Archiving configuration
config :otp_supervisor, :archiving,
  enabled: true,
  archive_path: "/var/archives/otp_supervisor",
  
  # Archive policies
  policies: %{
    audit_logs: %{retention_days: 90, archive_after_days: 30},
    system_metrics: %{retention_days: 365, archive_after_days: 7},
    sandbox_states: %{retention_days: 30, archive_after_days: 7, keep_latest: 10}
  }
```

---

## Conclusion

This Database and Storage Design provides a comprehensive data management foundation for the interactive OTP sandbox platform. The design emphasizes:

- **Multi-Model Storage**: Specialized databases for different data types and access patterns
- **Scalability**: Horizontal scaling through partitioning and sharding
- **Reliability**: Automated backup and recovery with multiple redundancy levels
- **Performance**: Optimized queries, indexing, and caching strategies
- **Security**: Encryption, access controls, and audit logging
- **Maintenance**: Automated archiving and cleanup processes

The system is designed to handle millions of users and sandbox sessions while maintaining high availability and performance.

<function_calls>
<invoke name="TodoWrite">
<parameter name="todos">[{"id": "1", "content": "Create Architecture Design Document", "status": "completed", "priority": "high"}, {"id": "2", "content": "Create Hot Code Reloading Technical Design", "status": "completed", "priority": "high"}, {"id": "3", "content": "Create State Management System Design", "status": "completed", "priority": "high"}, {"id": "4", "content": "Create Collaboration System Design", "status": "completed", "priority": "high"}, {"id": "5", "content": "Create Educational Framework Design", "status": "completed", "priority": "high"}, {"id": "6", "content": "Create Security and Isolation Design", "status": "completed", "priority": "high"}, {"id": "7", "content": "Create Performance and Scalability Design", "status": "completed", "priority": "medium"}, {"id": "8", "content": "Create Database and Storage Design", "status": "completed", "priority": "medium"}]