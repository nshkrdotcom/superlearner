# Educational Framework Design Document
## Interactive OTP Sandbox Development Platform

**Version**: 1.0  
**Date**: July 9, 2025  
**Authors**: System Architecture Team  
**Status**: Draft

---

## Table of Contents

1. [Overview](#overview)
2. [Learning Architecture](#learning-architecture)
3. [Core Components](#core-components)
4. [Tutorial System](#tutorial-system)
5. [Exercise Engine](#exercise-engine)
6. [Assessment Framework](#assessment-framework)
7. [Adaptive Learning](#adaptive-learning)
8. [Implementation Details](#implementation-details)
9. [Content Management](#content-management)
10. [Analytics and Progress Tracking](#analytics-and-progress-tracking)
11. [Testing Strategy](#testing-strategy)

---

## Overview

### Purpose

The Educational Framework transforms the OTP sandbox into an interactive learning platform that teaches OTP concepts through hands-on experience. It provides guided tutorials, practical exercises, automated assessment, and adaptive learning paths tailored to individual student needs.

### Design Goals

- **Interactive Learning**: Learn by doing with real OTP processes and systems
- **Adaptive Content**: Adjust difficulty and pacing based on student performance
- **Comprehensive Assessment**: Automated evaluation of code quality and understanding
- **Gamification**: Engagement through achievements, progress tracking, and challenges
- **Mentoring Support**: Tools for instructors to guide and assess students
- **Real-world Relevance**: Exercises based on actual OTP patterns and use cases

### Key Features

- Interactive tutorials with live code execution
- Hands-on exercises with automated feedback
- Pattern recognition and code analysis
- Adaptive difficulty adjustment
- Progress tracking and analytics
- Collaborative learning support
- Instructor dashboard and tools
- Achievement system and gamification

---

## Learning Architecture

### High-Level Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    Educational Framework                        │
├─────────────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────┐ │
│  │  Tutorial   │  │  Exercise   │  │ Assessment  │  │Adaptive │ │
│  │   System    │  │   Engine    │  │ Framework   │  │Learning │ │
│  └─────────────┘  └─────────────┘  └─────────────┘  └─────────┘ │
├─────────────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────┐ │
│  │  Pattern    │  │  Progress   │  │ Gamification│  │Content  │ │
│  │ Recognition │  │  Tracker    │  │   System    │  │Manager  │ │
│  └─────────────┘  └─────────────┘  └─────────────┘  └─────────┘ │
├─────────────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────┐ │
│  │ Instructor  │  │ Analytics   │  │ Collaboration│  │Student │ │
│  │ Dashboard   │  │ Engine      │  │ Learning    │  │Profile  │ │
│  └─────────────┘  └─────────────┘  └─────────────┘  └─────────┘ │
└─────────────────────────────────────────────────────────────────┘
```

### Learning Flow

```
Student Login → Assessment → Learning Path → Tutorial/Exercise → 
Evaluation → Feedback → Adaptation → Next Content → Progress Update
```

### Adaptive Learning Process

```
Student Action → Pattern Analysis → Skill Assessment → Difficulty Adjustment → 
Content Selection → Personalized Feedback → Progress Tracking
```

---

## Core Components

### 1. Tutorial System

**Purpose**: Provides interactive, step-by-step tutorials that teach OTP concepts through hands-on experience.

**Key Features**:
- Interactive code execution within tutorials
- Visual process demonstrations
- Checkpoint system for progress saving
- Adaptive explanations based on skill level
- Multi-modal content (text, video, interactive demos)

**Implementation**:

```elixir
defmodule OtpSupervisor.Education.TutorialSystem do
  @moduledoc """
  Manages interactive tutorials for OTP learning.
  """

  use GenServer
  require Logger

  defstruct [
    :tutorial_id,
    :student_id,
    :current_step,
    :tutorial_data,
    :student_progress,
    :sandbox_instance,
    :completion_status
  ]

  def start_link(tutorial_id, student_id) do
    GenServer.start_link(__MODULE__, {tutorial_id, student_id}, 
                        name: via_tuple(tutorial_id, student_id))
  end

  def start_tutorial(tutorial_id, student_id) do
    GenServer.call(via_tuple(tutorial_id, student_id), :start_tutorial)
  end

  def next_step(tutorial_id, student_id) do
    GenServer.call(via_tuple(tutorial_id, student_id), :next_step)
  end

  def submit_step_answer(tutorial_id, student_id, answer) do
    GenServer.call(via_tuple(tutorial_id, student_id), {:submit_answer, answer})
  end

  def get_tutorial_state(tutorial_id, student_id) do
    GenServer.call(via_tuple(tutorial_id, student_id), :get_state)
  end

  def init({tutorial_id, student_id}) do
    # Load tutorial content
    {:ok, tutorial_data} = load_tutorial_content(tutorial_id)
    
    # Get student progress
    student_progress = get_student_progress(student_id, tutorial_id)
    
    # Create dedicated sandbox instance
    {:ok, sandbox_instance} = create_tutorial_sandbox(tutorial_id, student_id)
    
    state = %__MODULE__{
      tutorial_id: tutorial_id,
      student_id: student_id,
      current_step: student_progress.current_step || 1,
      tutorial_data: tutorial_data,
      student_progress: student_progress,
      sandbox_instance: sandbox_instance,
      completion_status: %{}
    }

    {:ok, state}
  end

  def handle_call(:start_tutorial, _from, state) do
    # Initialize tutorial environment
    case setup_tutorial_environment(state) do
      {:ok, new_state} ->
        current_step_data = get_current_step_data(new_state)
        
        # Track tutorial start
        track_tutorial_event(state.student_id, state.tutorial_id, :started)
        
        response = %{
          status: :started,
          step: current_step_data,
          progress: calculate_progress(new_state)
        }
        
        {:reply, {:ok, response}, new_state}
      
      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call(:next_step, _from, state) do
    case can_advance_step?(state) do
      true ->
        new_current_step = state.current_step + 1
        
        if new_current_step <= length(state.tutorial_data.steps) do
          new_state = %{state | current_step: new_current_step}
          
          # Setup next step environment
          {:ok, prepared_state} = prepare_step_environment(new_state)
          
          current_step_data = get_current_step_data(prepared_state)
          
          # Save progress
          save_student_progress(state.student_id, state.tutorial_id, new_current_step)
          
          response = %{
            status: :step_advanced,
            step: current_step_data,
            progress: calculate_progress(prepared_state)
          }
          
          {:reply, {:ok, response}, prepared_state}
        else
          # Tutorial completed
          complete_tutorial(state)
        end
      
      false ->
        {:reply, {:error, :step_not_completed}, state}
    end
  end

  def handle_call({:submit_answer, answer}, _from, state) do
    current_step_data = get_current_step_data(state)
    
    case evaluate_step_answer(current_step_data, answer, state) do
      {:ok, evaluation} ->
        new_completion_status = Map.put(
          state.completion_status, 
          state.current_step, 
          evaluation
        )
        
        new_state = %{state | completion_status: new_completion_status}
        
        # Provide feedback
        feedback = generate_step_feedback(evaluation, current_step_data)
        
        # Track answer submission
        track_answer_submission(state.student_id, state.tutorial_id, 
                               state.current_step, answer, evaluation)
        
        response = %{
          status: :answer_evaluated,
          evaluation: evaluation,
          feedback: feedback,
          can_advance: evaluation.correct?
        }
        
        {:reply, {:ok, response}, new_state}
      
      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call(:get_state, _from, state) do
    current_step_data = get_current_step_data(state)
    
    response = %{
      tutorial_id: state.tutorial_id,
      student_id: state.student_id,
      current_step: state.current_step,
      total_steps: length(state.tutorial_data.steps),
      step_data: current_step_data,
      progress: calculate_progress(state),
      completion_status: state.completion_status
    }
    
    {:reply, response, state}
  end

  defp load_tutorial_content(tutorial_id) do
    case OtpSupervisor.Education.ContentManager.get_tutorial(tutorial_id) do
      {:ok, tutorial} -> {:ok, tutorial}
      {:error, reason} -> {:error, reason}
    end
  end

  defp get_student_progress(student_id, tutorial_id) do
    case OtpSupervisor.Education.ProgressTracker.get_tutorial_progress(
      student_id, tutorial_id
    ) do
      {:ok, progress} -> progress
      {:error, :not_found} -> %{current_step: 1, completed: false}
    end
  end

  defp create_tutorial_sandbox(tutorial_id, student_id) do
    sandbox_id = "tutorial_#{tutorial_id}_#{student_id}_#{System.unique_integer([:positive])}"
    
    # Create isolated sandbox for tutorial
    OtpSupervisor.Core.SandboxManager.create_sandbox(sandbox_id, %{
      type: :tutorial,
      tutorial_id: tutorial_id,
      student_id: student_id,
      restricted: true
    })
  end

  defp setup_tutorial_environment(state) do
    # Initialize sandbox with tutorial-specific code
    case prepare_tutorial_code(state) do
      {:ok, new_state} ->
        # Setup initial processes if needed
        setup_initial_processes(new_state)
      
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp prepare_tutorial_code(state) do
    tutorial_step = Enum.at(state.tutorial_data.steps, state.current_step - 1)
    
    case tutorial_step.initial_code do
      nil -> {:ok, state}
      code ->
        # Load initial code into sandbox
        case OtpSupervisor.Core.SandboxManager.load_code(
          state.sandbox_instance, code
        ) do
          :ok -> {:ok, state}
          {:error, reason} -> {:error, reason}
        end
    end
  end

  defp setup_initial_processes(state) do
    tutorial_step = Enum.at(state.tutorial_data.steps, state.current_step - 1)
    
    case tutorial_step.initial_processes do
      nil -> {:ok, state}
      processes ->
        # Start required processes
        case start_tutorial_processes(state.sandbox_instance, processes) do
          :ok -> {:ok, state}
          {:error, reason} -> {:error, reason}
        end
    end
  end

  defp start_tutorial_processes(sandbox_instance, processes) do
    Enum.reduce_while(processes, :ok, fn process_spec, :ok ->
      case start_single_process(sandbox_instance, process_spec) do
        :ok -> {:cont, :ok}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  defp start_single_process(sandbox_instance, process_spec) do
    OtpSupervisor.Core.SandboxManager.start_process(
      sandbox_instance, 
      process_spec.module, 
      process_spec.args || []
    )
  end

  defp get_current_step_data(state) do
    step_data = Enum.at(state.tutorial_data.steps, state.current_step - 1)
    
    # Personalize step content based on student level
    personalized_content = personalize_step_content(step_data, state.student_id)
    
    Map.merge(step_data, personalized_content)
  end

  defp personalize_step_content(step_data, student_id) do
    # Get student skill level
    skill_level = OtpSupervisor.Education.StudentProfile.get_skill_level(student_id)
    
    case skill_level do
      :beginner -> %{
        explanation: step_data.explanation_beginner || step_data.explanation,
        hints: step_data.hints_beginner || step_data.hints,
        examples: step_data.examples_detailed || step_data.examples
      }
      
      :intermediate -> %{
        explanation: step_data.explanation,
        hints: step_data.hints,
        examples: step_data.examples
      }
      
      :advanced -> %{
        explanation: step_data.explanation_advanced || step_data.explanation,
        hints: step_data.hints_minimal || [],
        examples: step_data.examples_minimal || step_data.examples
      }
    end
  end

  defp evaluate_step_answer(step_data, answer, state) do
    case step_data.evaluation_type do
      :code_execution ->
        evaluate_code_execution(step_data, answer, state)
      
      :pattern_matching ->
        evaluate_pattern_matching(step_data, answer, state)
      
      :multiple_choice ->
        evaluate_multiple_choice(step_data, answer, state)
      
      :process_state ->
        evaluate_process_state(step_data, answer, state)
      
      :custom ->
        evaluate_custom(step_data, answer, state)
    end
  end

  defp evaluate_code_execution(step_data, code_answer, state) do
    # Execute code in sandbox and check results
    case OtpSupervisor.Core.SandboxManager.execute_code(
      state.sandbox_instance, code_answer
    ) do
      {:ok, result} ->
        # Check if result matches expected outcome
        case check_execution_result(result, step_data.expected_result) do
          true ->
            {:ok, %{
              correct?: true,
              result: result,
              feedback: "Great! Your code executed correctly.",
              score: calculate_code_score(code_answer, step_data)
            }}
          
          false ->
            {:ok, %{
              correct?: false,
              result: result,
              expected: step_data.expected_result,
              feedback: generate_code_feedback(result, step_data),
              hints: provide_code_hints(code_answer, step_data)
            }}
        end
      
      {:error, reason} ->
        {:ok, %{
          correct?: false,
          error: reason,
          feedback: "Your code has an error. Please check and try again.",
          hints: provide_error_hints(reason, step_data)
        }}
    end
  end

  defp evaluate_pattern_matching(step_data, answer, _state) do
    # Use pattern recognition to evaluate answer
    case OtpSupervisor.Education.PatternRecognition.analyze_code(answer) do
      {:ok, patterns} ->
        required_patterns = step_data.required_patterns
        
        missing_patterns = required_patterns -- patterns
        extra_patterns = patterns -- required_patterns
        
        correct = length(missing_patterns) == 0
        
        {:ok, %{
          correct?: correct,
          patterns_found: patterns,
          missing_patterns: missing_patterns,
          extra_patterns: extra_patterns,
          feedback: generate_pattern_feedback(patterns, required_patterns)
        }}
      
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp evaluate_multiple_choice(step_data, answer, _state) do
    correct_answer = step_data.correct_answer
    
    {:ok, %{
      correct?: answer == correct_answer,
      selected: answer,
      correct_answer: correct_answer,
      feedback: get_choice_feedback(answer, step_data)
    }}
  end

  defp evaluate_process_state(step_data, answer, state) do
    # Check the state of processes in sandbox
    case OtpSupervisor.Core.SandboxManager.get_process_states(state.sandbox_instance) do
      {:ok, process_states} ->
        expected_state = step_data.expected_process_state
        
        matches = check_process_state_match(process_states, expected_state)
        
        {:ok, %{
          correct?: matches,
          current_state: process_states,
          expected_state: expected_state,
          feedback: generate_process_feedback(process_states, expected_state)
        }}
      
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp generate_step_feedback(evaluation, step_data) do
    base_feedback = if evaluation.correct? do
      "Excellent work! You've completed this step correctly."
    else
      "Not quite right. Let's review what went wrong."
    end
    
    # Add specific feedback based on evaluation type
    specific_feedback = case step_data.evaluation_type do
      :code_execution -> generate_code_feedback(evaluation, step_data)
      :pattern_matching -> generate_pattern_feedback(evaluation, step_data)
      :multiple_choice -> evaluation.feedback
      :process_state -> generate_process_feedback(evaluation, step_data)
      :custom -> evaluation.feedback
    end
    
    %{
      general: base_feedback,
      specific: specific_feedback,
      next_steps: get_next_steps_suggestion(evaluation, step_data)
    }
  end

  defp can_advance_step?(state) do
    case Map.get(state.completion_status, state.current_step) do
      nil -> false
      %{correct?: correct} -> correct
      _ -> false
    end
  end

  defp complete_tutorial(state) do
    # Mark tutorial as completed
    :ok = OtpSupervisor.Education.ProgressTracker.complete_tutorial(
      state.student_id, state.tutorial_id
    )
    
    # Calculate final score
    final_score = calculate_final_score(state)
    
    # Award achievements
    achievements = award_tutorial_achievements(state, final_score)
    
    # Track completion
    track_tutorial_event(state.student_id, state.tutorial_id, :completed, %{
      score: final_score,
      achievements: achievements
    })
    
    response = %{
      status: :tutorial_completed,
      final_score: final_score,
      achievements: achievements,
      next_recommendations: get_next_tutorial_recommendations(state)
    }
    
    {:reply, {:ok, response}, state}
  end

  defp calculate_progress(state) do
    total_steps = length(state.tutorial_data.steps)
    completed_steps = map_size(state.completion_status)
    
    %{
      current_step: state.current_step,
      total_steps: total_steps,
      completed_steps: completed_steps,
      percentage: (completed_steps / total_steps) * 100
    }
  end

  defp track_tutorial_event(student_id, tutorial_id, event_type, metadata \\ %{}) do
    OtpSupervisor.Education.Analytics.track_event(student_id, %{
      type: :tutorial_event,
      tutorial_id: tutorial_id,
      event: event_type,
      metadata: metadata,
      timestamp: System.monotonic_time()
    })
  end

  defp track_answer_submission(student_id, tutorial_id, step, answer, evaluation) do
    OtpSupervisor.Education.Analytics.track_event(student_id, %{
      type: :answer_submission,
      tutorial_id: tutorial_id,
      step: step,
      answer: answer,
      evaluation: evaluation,
      timestamp: System.monotonic_time()
    })
  end

  defp via_tuple(tutorial_id, student_id) do
    {:via, Registry, {OtpSupervisor.Education.TutorialRegistry, "#{tutorial_id}_#{student_id}"}}
  end
end
```

### 2. Exercise Engine

**Purpose**: Provides hands-on coding exercises with automated evaluation and feedback.

**Key Features**:
- Code execution and testing
- Automated test suite evaluation
- Progressive difficulty scaling
- Hint system with multiple levels
- Real-time feedback and suggestions

**Implementation**:

```elixir
defmodule OtpSupervisor.Education.ExerciseEngine do
  @moduledoc """
  Manages coding exercises with automated evaluation and feedback.
  """

  use GenServer
  require Logger

  defstruct [
    :exercise_id,
    :student_id,
    :exercise_data,
    :sandbox_instance,
    :submission_history,
    :current_attempt,
    :hint_level,
    :start_time
  ]

  def start_link(exercise_id, student_id) do
    GenServer.start_link(__MODULE__, {exercise_id, student_id}, 
                        name: via_tuple(exercise_id, student_id))
  end

  def start_exercise(exercise_id, student_id) do
    GenServer.call(via_tuple(exercise_id, student_id), :start_exercise)
  end

  def submit_solution(exercise_id, student_id, code) do
    GenServer.call(via_tuple(exercise_id, student_id), {:submit_solution, code})
  end

  def get_hint(exercise_id, student_id) do
    GenServer.call(via_tuple(exercise_id, student_id), :get_hint)
  end

  def run_tests(exercise_id, student_id, code) do
    GenServer.call(via_tuple(exercise_id, student_id), {:run_tests, code})
  end

  def init({exercise_id, student_id}) do
    # Load exercise content
    {:ok, exercise_data} = load_exercise_content(exercise_id)
    
    # Create sandbox instance
    {:ok, sandbox_instance} = create_exercise_sandbox(exercise_id, student_id)
    
    state = %__MODULE__{
      exercise_id: exercise_id,
      student_id: student_id,
      exercise_data: exercise_data,
      sandbox_instance: sandbox_instance,
      submission_history: [],
      current_attempt: 1,
      hint_level: 0,
      start_time: System.monotonic_time()
    }

    {:ok, state}
  end

  def handle_call(:start_exercise, _from, state) do
    # Setup exercise environment
    case setup_exercise_environment(state) do
      {:ok, new_state} ->
        # Track exercise start
        track_exercise_event(state.student_id, state.exercise_id, :started)
        
        response = %{
          status: :started,
          exercise: prepare_exercise_data(new_state),
          sandbox_id: new_state.sandbox_instance
        }
        
        {:reply, {:ok, response}, new_state}
      
      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:submit_solution, code}, _from, state) do
    # Evaluate the submitted solution
    case evaluate_solution(code, state) do
      {:ok, evaluation} ->
        # Update submission history
        submission = %{
          attempt: state.current_attempt,
          code: code,
          evaluation: evaluation,
          timestamp: System.monotonic_time()
        }
        
        new_submission_history = [submission | state.submission_history]
        new_state = %{state | 
          submission_history: new_submission_history,
          current_attempt: state.current_attempt + 1
        }
        
        # Track submission
        track_submission(state.student_id, state.exercise_id, submission)
        
        # Check if exercise is completed
        if evaluation.passed? do
          complete_exercise(new_state, evaluation)
        else
          response = %{
            status: :evaluation_complete,
            evaluation: evaluation,
            attempt: state.current_attempt,
            hints_available: hints_available?(new_state)
          }
          
          {:reply, {:ok, response}, new_state}
        end
      
      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call(:get_hint, _from, state) do
    case get_next_hint(state) do
      {:ok, hint} ->
        new_state = %{state | hint_level: state.hint_level + 1}
        
        # Track hint usage
        track_hint_usage(state.student_id, state.exercise_id, state.hint_level)
        
        response = %{
          status: :hint_provided,
          hint: hint,
          hint_level: new_state.hint_level,
          remaining_hints: remaining_hints(new_state)
        }
        
        {:reply, {:ok, response}, new_state}
      
      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:run_tests, code}, _from, state) do
    # Run tests without evaluating as submission
    case run_test_suite(code, state) do
      {:ok, test_results} ->
        response = %{
          status: :tests_complete,
          results: test_results,
          passed: test_results.passed,
          failed: test_results.failed,
          total: test_results.total
        }
        
        {:reply, {:ok, response}, state}
      
      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  defp load_exercise_content(exercise_id) do
    case OtpSupervisor.Education.ContentManager.get_exercise(exercise_id) do
      {:ok, exercise} -> {:ok, exercise}
      {:error, reason} -> {:error, reason}
    end
  end

  defp create_exercise_sandbox(exercise_id, student_id) do
    sandbox_id = "exercise_#{exercise_id}_#{student_id}_#{System.unique_integer([:positive])}"
    
    OtpSupervisor.Core.SandboxManager.create_sandbox(sandbox_id, %{
      type: :exercise,
      exercise_id: exercise_id,
      student_id: student_id,
      timeout: 300_000  # 5 minutes
    })
  end

  defp setup_exercise_environment(state) do
    # Load starter code
    case load_starter_code(state) do
      {:ok, new_state} ->
        # Setup test environment
        setup_test_environment(new_state)
      
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp load_starter_code(state) do
    case state.exercise_data.starter_code do
      nil -> {:ok, state}
      code ->
        case OtpSupervisor.Core.SandboxManager.load_code(
          state.sandbox_instance, code
        ) do
          :ok -> {:ok, state}
          {:error, reason} -> {:error, reason}
        end
    end
  end

  defp setup_test_environment(state) do
    # Load test modules
    case load_test_modules(state) do
      {:ok, new_state} ->
        # Setup any required processes
        setup_test_processes(new_state)
      
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp evaluate_solution(code, state) do
    # First, run syntax and compilation checks
    case check_syntax_and_compile(code, state) do
      {:ok, _} ->
        # Run test suite
        case run_test_suite(code, state) do
          {:ok, test_results} ->
            # Analyze code quality
            quality_analysis = analyze_code_quality(code, state)
            
            # Check for required patterns
            pattern_analysis = check_required_patterns(code, state)
            
            # Calculate overall score
            score = calculate_exercise_score(test_results, quality_analysis, pattern_analysis)
            
            evaluation = %{
              passed?: test_results.passed == test_results.total,
              test_results: test_results,
              quality_analysis: quality_analysis,
              pattern_analysis: pattern_analysis,
              score: score,
              feedback: generate_comprehensive_feedback(
                test_results, quality_analysis, pattern_analysis, state
              )
            }
            
            {:ok, evaluation}
          
          {:error, reason} ->
            {:error, reason}
        end
      
      {:error, compilation_error} ->
        {:ok, %{
          passed?: false,
          compilation_error: compilation_error,
          feedback: generate_compilation_feedback(compilation_error, state)
        }}
    end
  end

  defp check_syntax_and_compile(code, state) do
    case OtpSupervisor.Core.SandboxManager.compile_code(state.sandbox_instance, code) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, reason}
    end
  end

  defp run_test_suite(code, state) do
    # Execute tests in sandbox
    test_command = build_test_command(code, state.exercise_data)
    
    case OtpSupervisor.Core.SandboxManager.execute_command(
      state.sandbox_instance, test_command
    ) do
      {:ok, result} ->
        # Parse test results
        parsed_results = parse_test_results(result, state.exercise_data)
        {:ok, parsed_results}
      
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp build_test_command(code, exercise_data) do
    case exercise_data.test_framework do
      :ex_unit ->
        "mix test --formatter OtpSupervisor.Education.TestFormatter"
      
      :property_based ->
        "mix test --include property_based"
      
      :custom ->
        exercise_data.custom_test_command
    end
  end

  defp parse_test_results(test_output, exercise_data) do
    # Parse test framework output
    case exercise_data.test_framework do
      :ex_unit -> parse_ex_unit_results(test_output)
      :property_based -> parse_property_test_results(test_output)
      :custom -> parse_custom_test_results(test_output, exercise_data)
    end
  end

  defp parse_ex_unit_results(test_output) do
    # Parse ExUnit output format
    lines = String.split(test_output, "\n")
    
    # Extract test counts
    summary_line = Enum.find(lines, &String.contains?(&1, "test"))
    
    case Regex.run(~r/(\d+) tests?, (\d+) failures?/, summary_line) do
      [_, total, failed] ->
        total_count = String.to_integer(total)
        failed_count = String.to_integer(failed)
        
        %{
          total: total_count,
          passed: total_count - failed_count,
          failed: failed_count,
          details: extract_test_details(lines)
        }
      
      _ ->
        %{total: 0, passed: 0, failed: 0, details: [], error: "Could not parse test results"}
    end
  end

  defp analyze_code_quality(code, state) do
    # Use pattern recognition to analyze code quality
    case OtpSupervisor.Education.PatternRecognition.analyze_code(code) do
      {:ok, analysis} ->
        quality_metrics = %{
          complexity: analysis.complexity,
          readability: analysis.readability,
          otp_patterns: analysis.otp_patterns,
          best_practices: analysis.best_practices,
          performance: analysis.performance
        }
        
        # Generate quality feedback
        feedback = generate_quality_feedback(quality_metrics, state.exercise_data)
        
        Map.put(quality_metrics, :feedback, feedback)
      
      {:error, _} ->
        %{error: "Could not analyze code quality"}
    end
  end

  defp check_required_patterns(code, state) do
    required_patterns = state.exercise_data.required_patterns || []
    
    case OtpSupervisor.Education.PatternRecognition.check_patterns(code, required_patterns) do
      {:ok, pattern_results} ->
        missing_patterns = Enum.filter(required_patterns, fn pattern ->
          not Enum.member?(pattern_results.found_patterns, pattern)
        end)
        
        %{
          required_patterns: required_patterns,
          found_patterns: pattern_results.found_patterns,
          missing_patterns: missing_patterns,
          pattern_score: calculate_pattern_score(pattern_results, required_patterns)
        }
      
      {:error, reason} ->
        %{error: reason}
    end
  end

  defp calculate_exercise_score(test_results, quality_analysis, pattern_analysis) do
    # Test score (40% weight)
    test_score = if test_results.total > 0 do
      (test_results.passed / test_results.total) * 40
    else
      0
    end
    
    # Quality score (30% weight)
    quality_score = case quality_analysis do
      %{complexity: complexity, readability: readability} ->
        quality_average = (complexity + readability) / 2
        quality_average * 30
      
      _ -> 0
    end
    
    # Pattern score (30% weight)
    pattern_score = case pattern_analysis do
      %{pattern_score: score} -> score * 30
      _ -> 0
    end
    
    # Total score
    total = test_score + quality_score + pattern_score
    
    %{
      total: total,
      test_score: test_score,
      quality_score: quality_score,
      pattern_score: pattern_score,
      breakdown: %{
        tests: "#{test_results.passed}/#{test_results.total} tests passed",
        quality: "Code quality: #{quality_score}/30",
        patterns: "Required patterns: #{pattern_score}/30"
      }
    }
  end

  defp generate_comprehensive_feedback(test_results, quality_analysis, pattern_analysis, state) do
    feedback_sections = []
    
    # Test feedback
    test_feedback = generate_test_feedback(test_results, state)
    feedback_sections = [test_feedback | feedback_sections]
    
    # Quality feedback
    if Map.has_key?(quality_analysis, :feedback) do
      feedback_sections = [quality_analysis.feedback | feedback_sections]
    end
    
    # Pattern feedback
    pattern_feedback = generate_pattern_feedback(pattern_analysis, state)
    feedback_sections = [pattern_feedback | feedback_sections]
    
    # Combine all feedback
    %{
      sections: Enum.reverse(feedback_sections),
      overall: generate_overall_feedback(test_results, quality_analysis, pattern_analysis),
      next_steps: suggest_next_steps(test_results, quality_analysis, pattern_analysis)
    }
  end

  defp get_next_hint(state) do
    hints = state.exercise_data.hints || []
    
    if state.hint_level < length(hints) do
      hint = Enum.at(hints, state.hint_level)
      {:ok, hint}
    else
      {:error, :no_more_hints}
    end
  end

  defp complete_exercise(state, evaluation) do
    # Calculate time taken
    time_taken = System.monotonic_time() - state.start_time
    
    # Update student progress
    :ok = OtpSupervisor.Education.ProgressTracker.complete_exercise(
      state.student_id, state.exercise_id, evaluation.score.total
    )
    
    # Award achievements
    achievements = award_exercise_achievements(state, evaluation, time_taken)
    
    # Track completion
    track_exercise_event(state.student_id, state.exercise_id, :completed, %{
      score: evaluation.score.total,
      attempts: state.current_attempt,
      time_taken: time_taken,
      achievements: achievements
    })
    
    response = %{
      status: :exercise_completed,
      evaluation: evaluation,
      time_taken: time_taken,
      attempts: state.current_attempt,
      achievements: achievements,
      next_recommendations: get_next_exercise_recommendations(state)
    }
    
    {:reply, {:ok, response}, state}
  end

  defp track_exercise_event(student_id, exercise_id, event_type, metadata \\ %{}) do
    OtpSupervisor.Education.Analytics.track_event(student_id, %{
      type: :exercise_event,
      exercise_id: exercise_id,
      event: event_type,
      metadata: metadata,
      timestamp: System.monotonic_time()
    })
  end

  defp track_submission(student_id, exercise_id, submission) do
    OtpSupervisor.Education.Analytics.track_event(student_id, %{
      type: :exercise_submission,
      exercise_id: exercise_id,
      submission: submission,
      timestamp: System.monotonic_time()
    })
  end

  defp track_hint_usage(student_id, exercise_id, hint_level) do
    OtpSupervisor.Education.Analytics.track_event(student_id, %{
      type: :hint_usage,
      exercise_id: exercise_id,
      hint_level: hint_level,
      timestamp: System.monotonic_time()
    })
  end

  defp via_tuple(exercise_id, student_id) do
    {:via, Registry, {OtpSupervisor.Education.ExerciseRegistry, "#{exercise_id}_#{student_id}"}}
  end
end
```

### 3. Pattern Recognition System

**Purpose**: Analyzes student code to identify OTP patterns and provide targeted feedback.

**Key Features**:
- AST analysis for pattern detection
- OTP best practices checking
- Code complexity analysis
- Style and readability assessment
- Performance pattern recognition

**Implementation**:

```elixir
defmodule OtpSupervisor.Education.PatternRecognition do
  @moduledoc """
  Analyzes code patterns and provides educational feedback.
  """

  @otp_patterns [
    :gen_server,
    :gen_statem,
    :gen_event,
    :supervisor,
    :application,
    :process_registry,
    :message_passing,
    :error_handling,
    :supervision_tree,
    :hot_code_upgrade
  ]

  @best_practices [
    :proper_error_handling,
    :pattern_matching,
    :immutability,
    :tail_recursion,
    :proper_naming,
    :documentation,
    :testing,
    :logging
  ]

  def analyze_code(code) do
    try do
      # Parse code into AST
      case Code.string_to_quoted(code) do
        {:ok, ast} ->
          analysis = %{
            otp_patterns: detect_otp_patterns(ast),
            best_practices: check_best_practices(ast),
            complexity: calculate_complexity(ast),
            readability: assess_readability(ast),
            performance: analyze_performance(ast),
            suggestions: generate_suggestions(ast)
          }
          
          {:ok, analysis}
        
        {:error, reason} ->
          {:error, :parse_error, reason}
      end
    rescue
      error -> {:error, :analysis_error, error}
    end
  end

  def check_patterns(code, required_patterns) do
    case analyze_code(code) do
      {:ok, analysis} ->
        found_patterns = analysis.otp_patterns
        
        pattern_results = %{
          found_patterns: found_patterns,
          missing_patterns: required_patterns -- found_patterns,
          extra_patterns: found_patterns -- required_patterns,
          pattern_details: get_pattern_details(found_patterns, analysis)
        }
        
        {:ok, pattern_results}
      
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp detect_otp_patterns(ast) do
    patterns = []
    
    # Check for GenServer patterns
    patterns = if has_genserver_pattern?(ast) do
      [:gen_server | patterns]
    else
      patterns
    end
    
    # Check for Supervisor patterns
    patterns = if has_supervisor_pattern?(ast) do
      [:supervisor | patterns]
    else
      patterns
    end
    
    # Check for GenStatem patterns
    patterns = if has_genstatem_pattern?(ast) do
      [:gen_statem | patterns]
    else
      patterns
    end
    
    # Check for message passing patterns
    patterns = if has_message_passing_pattern?(ast) do
      [:message_passing | patterns]
    else
      patterns
    end
    
    # Check for error handling patterns
    patterns = if has_error_handling_pattern?(ast) do
      [:error_handling | patterns]
    else
      patterns
    end
    
    patterns
  end

  defp has_genserver_pattern?(ast) do
    # Look for GenServer behavior and callbacks
    has_behavior?(ast, :gen_server) or
    has_callback?(ast, :init) or
    has_callback?(ast, :handle_call) or
    has_callback?(ast, :handle_cast) or
    has_callback?(ast, :handle_info)
  end

  defp has_supervisor_pattern?(ast) do
    # Look for Supervisor behavior and callbacks
    has_behavior?(ast, :supervisor) or
    has_callback?(ast, :init) and has_childspec?(ast)
  end

  defp has_genstatem_pattern?(ast) do
    # Look for GenStatem behavior and callbacks
    has_behavior?(ast, :gen_statem) or
    has_callback?(ast, :callback_mode) or
    has_state_function_pattern?(ast)
  end

  defp has_message_passing_pattern?(ast) do
    # Look for send/receive patterns
    has_send_pattern?(ast) or has_receive_pattern?(ast)
  end

  defp has_error_handling_pattern?(ast) do
    # Look for try/catch, case statements with error tuples
    has_try_catch?(ast) or
    has_error_tuple_pattern?(ast) or
    has_with_pattern?(ast)
  end

  defp has_behavior?(ast, behavior) do
    # Check for @behaviour declarations
    Macro.postwalk(ast, false, fn
      {:@, _, [{:behaviour, _, [^behavior]}]}, _ -> {nil, true}
      node, acc -> {node, acc}
    end)
    |> elem(1)
  end

  defp has_callback?(ast, callback_name) do
    # Check for callback function definitions
    Macro.postwalk(ast, false, fn
      {:def, _, [{^callback_name, _, _} | _]}, _ -> {nil, true}
      node, acc -> {node, acc}
    end)
    |> elem(1)
  end

  defp has_childspec?(ast) do
    # Check for child specifications
    Macro.postwalk(ast, false, fn
      {:%{}, _, children}, acc when is_list(children) ->
        if Enum.any?(children, fn
          {key, _} when key in [:id, :start, :restart, :shutdown, :type, :modules] -> true
          _ -> false
        end) do
          {nil, true}
        else
          {nil, acc}
        end
      
      node, acc -> {node, acc}
    end)
    |> elem(1)
  end

  defp has_send_pattern?(ast) do
    # Check for send/1 calls
    Macro.postwalk(ast, false, fn
      {{:., _, [:erlang, :send]}, _, _}, _ -> {nil, true}
      {:send, _, _}, _ -> {nil, true}
      node, acc -> {node, acc}
    end)
    |> elem(1)
  end

  defp has_receive_pattern?(ast) do
    # Check for receive blocks
    Macro.postwalk(ast, false, fn
      {:receive, _, _}, _ -> {nil, true}
      node, acc -> {node, acc}
    end)
    |> elem(1)
  end

  defp has_try_catch?(ast) do
    # Check for try/catch blocks
    Macro.postwalk(ast, false, fn
      {:try, _, _}, _ -> {nil, true}
      node, acc -> {node, acc}
    end)
    |> elem(1)
  end

  defp has_error_tuple_pattern?(ast) do
    # Check for {:error, _} or {:ok, _} patterns
    Macro.postwalk(ast, false, fn
      {:{}, _, [:error, _]}, _ -> {nil, true}
      {:{}, _, [:ok, _]}, _ -> {nil, true}
      node, acc -> {node, acc}
    end)
    |> elem(1)
  end

  defp has_with_pattern?(ast) do
    # Check for with statements
    Macro.postwalk(ast, false, fn
      {:with, _, _}, _ -> {nil, true}
      node, acc -> {node, acc}
    end)
    |> elem(1)
  end

  defp check_best_practices(ast) do
    practices = []
    
    # Check for proper error handling
    practices = if has_proper_error_handling?(ast) do
      [:proper_error_handling | practices]
    else
      practices
    end
    
    # Check for pattern matching usage
    practices = if uses_pattern_matching?(ast) do
      [:pattern_matching | practices]
    else
      practices
    end
    
    # Check for immutability
    practices = if follows_immutability?(ast) do
      [:immutability | practices]
    else
      practices
    end
    
    # Check for tail recursion
    practices = if uses_tail_recursion?(ast) do
      [:tail_recursion | practices]
    else
      practices
    end
    
    # Check for proper naming
    practices = if has_proper_naming?(ast) do
      [:proper_naming | practices]
    else
      practices
    end
    
    # Check for documentation
    practices = if has_documentation?(ast) do
      [:documentation | practices]
    else
      practices
    end
    
    practices
  end

  defp calculate_complexity(ast) do
    # Calculate cyclomatic complexity
    complexity = Macro.postwalk(ast, 0, fn
      {:if, _, _}, acc -> {nil, acc + 1}
      {:case, _, _}, acc -> {nil, acc + 1}
      {:cond, _, _}, acc -> {nil, acc + 1}
      {:try, _, _}, acc -> {nil, acc + 1}
      {:receive, _, _}, acc -> {nil, acc + 1}
      {:fn, _, _}, acc -> {nil, acc + 1}
      node, acc -> {node, acc}
    end)
    |> elem(1)
    
    # Normalize complexity score (0-100)
    normalized = min(100, max(0, (100 - complexity * 5)))
    
    %{
      cyclomatic_complexity: complexity,
      normalized_score: normalized,
      level: complexity_level(complexity)
    }
  end

  defp complexity_level(complexity) do
    cond do
      complexity <= 5 -> :low
      complexity <= 10 -> :medium
      complexity <= 20 -> :high
      true -> :very_high
    end
  end

  defp assess_readability(ast) do
    # Assess various readability factors
    factors = %{
      function_length: assess_function_length(ast),
      nesting_depth: assess_nesting_depth(ast),
      variable_naming: assess_variable_naming(ast),
      line_length: assess_line_length(ast)
    }
    
    # Calculate overall readability score
    total_score = factors
    |> Map.values()
    |> Enum.sum()
    |> div(map_size(factors))
    
    Map.put(factors, :overall_score, total_score)
  end

  defp assess_function_length(ast) do
    # Count lines in functions and penalize long functions
    function_lengths = extract_function_lengths(ast)
    
    if Enum.empty?(function_lengths) do
      100
    else
      avg_length = Enum.sum(function_lengths) / length(function_lengths)
      
      cond do
        avg_length <= 10 -> 100
        avg_length <= 20 -> 80
        avg_length <= 30 -> 60
        avg_length <= 50 -> 40
        true -> 20
      end
    end
  end

  defp assess_nesting_depth(ast) do
    # Calculate maximum nesting depth
    max_depth = calculate_max_nesting_depth(ast)
    
    cond do
      max_depth <= 3 -> 100
      max_depth <= 5 -> 80
      max_depth <= 7 -> 60
      max_depth <= 10 -> 40
      true -> 20
    end
  end

  defp assess_variable_naming(ast) do
    # Check variable naming conventions
    variable_names = extract_variable_names(ast)
    
    good_names = Enum.count(variable_names, &good_variable_name?/1)
    
    if length(variable_names) == 0 do
      100
    else
      (good_names / length(variable_names)) * 100
    end
  end

  defp good_variable_name?(name) when is_atom(name) do
    name_str = Atom.to_string(name)
    
    # Check for descriptive names (not single letters, not generic)
    String.length(name_str) > 1 and
    not String.match?(name_str, ~r/^[a-z]$/) and
    not Enum.member?(["x", "y", "z", "temp", "tmp", "val", "var"], name_str)
  end

  defp analyze_performance(ast) do
    # Analyze performance-related patterns
    issues = []
    
    # Check for inefficient patterns
    issues = if has_inefficient_list_operations?(ast) do
      [:inefficient_list_operations | issues]
    else
      issues
    end
    
    # Check for unnecessary recursion
    issues = if has_unnecessary_recursion?(ast) do
      [:unnecessary_recursion | issues]
    else
      issues
    end
    
    # Check for memory leaks
    issues = if has_potential_memory_leaks?(ast) do
      [:potential_memory_leaks | issues]
    else
      issues
    end
    
    # Calculate performance score
    performance_score = max(0, 100 - (length(issues) * 20))
    
    %{
      issues: issues,
      score: performance_score,
      suggestions: generate_performance_suggestions(issues)
    }
  end

  defp generate_suggestions(ast) do
    suggestions = []
    
    # Analyze patterns and generate specific suggestions
    analysis = %{
      otp_patterns: detect_otp_patterns(ast),
      best_practices: check_best_practices(ast),
      complexity: calculate_complexity(ast),
      readability: assess_readability(ast),
      performance: analyze_performance(ast)
    }
    
    # Generate suggestions based on analysis
    suggestions = suggestions ++ generate_otp_suggestions(analysis)
    suggestions = suggestions ++ generate_best_practice_suggestions(analysis)
    suggestions = suggestions ++ generate_complexity_suggestions(analysis)
    suggestions = suggestions ++ generate_readability_suggestions(analysis)
    suggestions = suggestions ++ generate_performance_suggestions(analysis.performance.issues)
    
    suggestions
  end

  defp generate_otp_suggestions(analysis) do
    suggestions = []
    
    # Suggest OTP patterns if appropriate
    if :gen_server not in analysis.otp_patterns and might_benefit_from_genserver?(analysis) do
      suggestions = ["Consider using GenServer for state management" | suggestions]
    end
    
    if :supervisor not in analysis.otp_patterns and might_benefit_from_supervisor?(analysis) do
      suggestions = ["Consider using Supervisor for fault tolerance" | suggestions]
    end
    
    if :error_handling not in analysis.otp_patterns do
      suggestions = ["Improve error handling with proper error tuples" | suggestions]
    end
    
    suggestions
  end

  defp generate_best_practice_suggestions(analysis) do
    suggestions = []
    
    if :pattern_matching not in analysis.best_practices do
      suggestions = ["Use more pattern matching for cleaner code" | suggestions]
    end
    
    if :proper_naming not in analysis.best_practices do
      suggestions = ["Use more descriptive variable and function names" | suggestions]
    end
    
    if :documentation not in analysis.best_practices do
      suggestions = ["Add documentation with @doc and @moduledoc" | suggestions]
    end
    
    suggestions
  end

  defp generate_complexity_suggestions(analysis) do
    suggestions = []
    
    if analysis.complexity.level in [:high, :very_high] do
      suggestions = ["Consider breaking down complex functions into smaller ones" | suggestions]
      suggestions = ["Reduce nesting depth with early returns or helper functions" | suggestions]
    end
    
    suggestions
  end

  defp generate_readability_suggestions(analysis) do
    suggestions = []
    
    if analysis.readability.overall_score < 70 do
      suggestions = ["Improve code readability with better formatting" | suggestions]
    end
    
    if analysis.readability.function_length < 60 do
      suggestions = ["Break down long functions into smaller, focused functions" | suggestions]
    end
    
    if analysis.readability.nesting_depth < 60 do
      suggestions = ["Reduce nesting depth for better readability" | suggestions]
    end
    
    suggestions
  end

  defp generate_performance_suggestions(issues) do
    Enum.map(issues, fn issue ->
      case issue do
        :inefficient_list_operations -> "Use more efficient list operations (consider Enum.reduce instead of ++)"
        :unnecessary_recursion -> "Consider using built-in functions instead of manual recursion"
        :potential_memory_leaks -> "Check for processes that might not terminate properly"
        _ -> "Consider optimizing performance-critical code"
      end
    end)
  end

  # Helper functions for pattern detection
  defp might_benefit_from_genserver?(analysis) do
    # Heuristic: if there's state management and message passing
    :message_passing in analysis.otp_patterns and
    has_state_management_pattern?(analysis)
  end

  defp might_benefit_from_supervisor?(analysis) do
    # Heuristic: if there are processes being started
    :message_passing in analysis.otp_patterns and
    has_process_starting_pattern?(analysis)
  end

  defp has_state_management_pattern?(analysis) do
    # Check for state-related patterns in the analysis
    # This would be more sophisticated in a real implementation
    false
  end

  defp has_process_starting_pattern?(analysis) do
    # Check for process starting patterns
    # This would be more sophisticated in a real implementation
    false
  end

  # Additional helper functions would be implemented here
  defp has_proper_error_handling?(ast), do: has_error_tuple_pattern?(ast)
  defp uses_pattern_matching?(ast), do: has_pattern_matching?(ast)
  defp follows_immutability?(ast), do: true  # Simplified
  defp uses_tail_recursion?(ast), do: has_tail_recursion?(ast)
  defp has_proper_naming?(ast), do: assess_variable_naming(ast) > 70
  defp has_documentation?(ast), do: has_doc_attributes?(ast)
  
  defp has_pattern_matching?(ast) do
    # Check for pattern matching usage
    Macro.postwalk(ast, false, fn
      {:=, _, _}, _ -> {nil, true}
      {:case, _, _}, _ -> {nil, true}
      {:fn, _, clauses}, _ when is_list(clauses) -> {nil, true}
      node, acc -> {node, acc}
    end)
    |> elem(1)
  end
  
  defp has_tail_recursion?(ast) do
    # Simplified tail recursion detection
    Macro.postwalk(ast, false, fn
      {:def, _, [{name, _, _} | _]} = node, acc ->
        if has_tail_recursive_call?(node, name) do
          {nil, true}
        else
          {node, acc}
        end
      
      node, acc -> {node, acc}
    end)
    |> elem(1)
  end
  
  defp has_tail_recursive_call?(ast, function_name) do
    # Check if function calls itself in tail position
    # This is a simplified implementation
    false
  end
  
  defp has_doc_attributes?(ast) do
    Macro.postwalk(ast, false, fn
      {:@, _, [{:doc, _, _}]}, _ -> {nil, true}
      {:@, _, [{:moduledoc, _, _}]}, _ -> {nil, true}
      node, acc -> {node, acc}
    end)
    |> elem(1)
  end
  
  defp has_inefficient_list_operations?(ast) do
    # Check for inefficient list operations like excessive use of ++
    Macro.postwalk(ast, false, fn
      {:++, _, _}, _ -> {nil, true}
      node, acc -> {node, acc}
    end)
    |> elem(1)
  end
  
  defp has_unnecessary_recursion?(ast) do
    # Check for patterns that could use built-in functions
    # This is a simplified check
    false
  end
  
  defp has_potential_memory_leaks?(ast) do
    # Check for patterns that might cause memory leaks
    # This is a simplified check
    false
  end
  
  defp extract_function_lengths(ast) do
    # Extract function lengths (simplified)
    []
  end
  
  defp calculate_max_nesting_depth(ast) do
    # Calculate maximum nesting depth
    Macro.postwalk(ast, {0, 0}, fn
      node, {current_depth, max_depth} when node in [:if, :case, :cond, :try, :receive, :fn] ->
        new_depth = current_depth + 1
        {nil, {new_depth, max(max_depth, new_depth)}}
      
      node, {current_depth, max_depth} ->
        {node, {current_depth, max_depth}}
    end)
    |> elem(1)
    |> elem(1)
  end
  
  defp extract_variable_names(ast) do
    # Extract variable names from AST
    Macro.postwalk(ast, [], fn
      {var_name, _, nil}, acc when is_atom(var_name) ->
        {nil, [var_name | acc]}
      
      node, acc -> {node, acc}
    end)
    |> elem(1)
    |> Enum.uniq()
  end
  
  defp assess_line_length(ast) do
    # This would need the original source code to assess line length
    # For now, return a neutral score
    80
  end
  
  defp get_pattern_details(found_patterns, analysis) do
    # Get detailed information about found patterns
    Enum.map(found_patterns, fn pattern ->
      %{
        pattern: pattern,
        description: get_pattern_description(pattern),
        quality: assess_pattern_quality(pattern, analysis)
      }
    end)
  end
  
  defp get_pattern_description(pattern) do
    case pattern do
      :gen_server -> "GenServer behavior for stateful processes"
      :supervisor -> "Supervisor for fault tolerance"
      :gen_statem -> "GenStatem for state machine implementation"
      :message_passing -> "Message passing between processes"
      :error_handling -> "Proper error handling patterns"
      _ -> "OTP pattern: #{pattern}"
    end
  end
  
  defp assess_pattern_quality(pattern, analysis) do
    # Assess how well the pattern is implemented
    # This would be more sophisticated in a real implementation
    %{
      score: 80,
      suggestions: []
    }
  end
end
```

---

## Adaptive Learning System

### Learning Path Engine

```elixir
defmodule OtpSupervisor.Education.AdaptiveLearning do
  @moduledoc """
  Manages adaptive learning paths based on student performance and preferences.
  """

  use GenServer
  require Logger

  defstruct [
    :student_id,
    :learning_profile,
    :current_path,
    :completed_content,
    :skill_levels,
    :preferences,
    :performance_history
  ]

  def start_link(student_id) do
    GenServer.start_link(__MODULE__, student_id, name: via_tuple(student_id))
  end

  def get_next_content(student_id) do
    GenServer.call(via_tuple(student_id), :get_next_content)
  end

  def update_performance(student_id, content_id, performance_data) do
    GenServer.cast(via_tuple(student_id), {:update_performance, content_id, performance_data})
  end

  def adjust_difficulty(student_id, adjustment) do
    GenServer.cast(via_tuple(student_id), {:adjust_difficulty, adjustment})
  end

  def init(student_id) do
    # Load student learning profile
    learning_profile = load_learning_profile(student_id)
    
    # Initialize skill levels
    skill_levels = initialize_skill_levels(learning_profile)
    
    # Generate initial learning path
    initial_path = generate_learning_path(skill_levels, learning_profile)
    
    state = %__MODULE__{
      student_id: student_id,
      learning_profile: learning_profile,
      current_path: initial_path,
      completed_content: MapSet.new(),
      skill_levels: skill_levels,
      preferences: learning_profile.preferences,
      performance_history: []
    }

    {:ok, state}
  end

  def handle_call(:get_next_content, _from, state) do
    case select_next_content(state) do
      {:ok, content, new_state} ->
        {:reply, {:ok, content}, new_state}
      
      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_cast({:update_performance, content_id, performance_data}, state) do
    # Update performance history
    new_performance_history = [
      {content_id, performance_data, System.monotonic_time()} | state.performance_history
    ]
    
    # Update skill levels based on performance
    new_skill_levels = update_skill_levels(state.skill_levels, content_id, performance_data)
    
    # Mark content as completed if passed
    new_completed_content = if performance_data.passed? do
      MapSet.put(state.completed_content, content_id)
    else
      state.completed_content
    end
    
    # Regenerate learning path if needed
    new_path = if should_regenerate_path?(state, performance_data) do
      generate_learning_path(new_skill_levels, state.learning_profile)
    else
      state.current_path
    end
    
    new_state = %{state |
      performance_history: new_performance_history,
      skill_levels: new_skill_levels,
      completed_content: new_completed_content,
      current_path: new_path
    }
    
    {:noreply, new_state}
  end

  def handle_cast({:adjust_difficulty, adjustment}, state) do
    # Adjust difficulty preference
    new_preferences = Map.put(state.preferences, :difficulty_preference, adjustment)
    
    # Update learning profile
    new_learning_profile = %{state.learning_profile | preferences: new_preferences}
    
    # Regenerate learning path with new difficulty
    new_path = generate_learning_path(state.skill_levels, new_learning_profile)
    
    new_state = %{state |
      preferences: new_preferences,
      learning_profile: new_learning_profile,
      current_path: new_path
    }
    
    {:noreply, new_state}
  end

  defp load_learning_profile(student_id) do
    case OtpSupervisor.Education.StudentProfile.get_profile(student_id) do
      {:ok, profile} -> profile
      {:error, :not_found} -> create_default_profile(student_id)
    end
  end

  defp create_default_profile(student_id) do
    %{
      student_id: student_id,
      learning_style: :balanced,
      preferences: %{
        difficulty_preference: :medium,
        content_type_preference: :mixed,
        pacing: :normal,
        feedback_style: :detailed
      },
      initial_assessment: nil,
      goals: []
    }
  end

  defp initialize_skill_levels(learning_profile) do
    case learning_profile.initial_assessment do
      nil ->
        # Default skill levels for new students
        %{
          otp_basics: :beginner,
          gen_server: :beginner,
          supervisor: :beginner,
          gen_statem: :beginner,
          applications: :beginner,
          testing: :beginner,
          debugging: :beginner,
          performance: :beginner
        }
      
      assessment ->
        # Use assessment results to set skill levels
        assessment.skill_levels
    end
  end

  defp generate_learning_path(skill_levels, learning_profile) do
    # Get available content
    available_content = OtpSupervisor.Education.ContentManager.get_all_content()
    
    # Filter content based on skill levels and preferences
    suitable_content = filter_suitable_content(available_content, skill_levels, learning_profile)
    
    # Order content by learning progression
    ordered_content = order_by_learning_progression(suitable_content, skill_levels)
    
    # Apply preferences (difficulty, content type, etc.)
    personalized_content = apply_preferences(ordered_content, learning_profile.preferences)
    
    %{
      content_sequence: personalized_content,
      generated_at: System.monotonic_time(),
      skill_levels: skill_levels,
      preferences: learning_profile.preferences
    }
  end

  defp select_next_content(state) do
    # Find next content that hasn't been completed
    next_content = 
      state.current_path.content_sequence
      |> Enum.find(fn content ->
        not MapSet.member?(state.completed_content, content.id)
      end)
    
    case next_content do
      nil ->
        # No more content in current path, generate new path
        case generate_advanced_path(state) do
          {:ok, new_path} ->
            new_state = %{state | current_path: new_path}
            first_content = List.first(new_path.content_sequence)
            {:ok, first_content, new_state}
          
          {:error, reason} ->
            {:error, reason}
        end
      
      content ->
        # Return next content
        {:ok, content, state}
    end
  end

  defp filter_suitable_content(available_content, skill_levels, learning_profile) do
    available_content
    |> Enum.filter(fn content ->
      # Check if content is appropriate for current skill level
      content_skill_level = Map.get(skill_levels, content.skill_area, :beginner)
      content_difficulty = content.difficulty
      
      is_appropriate_difficulty?(content_difficulty, content_skill_level, learning_profile.preferences)
    end)
    |> Enum.filter(fn content ->
      # Check if prerequisites are met
      prerequisites_met?(content.prerequisites, skill_levels)
    end)
  end

  defp is_appropriate_difficulty?(content_difficulty, skill_level, preferences) do
    difficulty_preference = Map.get(preferences, :difficulty_preference, :medium)
    
    case {skill_level, difficulty_preference} do
      {:beginner, :easy} -> content_difficulty in [:beginner, :easy]
      {:beginner, :medium} -> content_difficulty in [:beginner, :intermediate]
      {:beginner, :hard} -> content_difficulty in [:beginner, :intermediate, :advanced]
      
      {:intermediate, :easy} -> content_difficulty in [:beginner, :intermediate]
      {:intermediate, :medium} -> content_difficulty in [:intermediate, :advanced]
      {:intermediate, :hard} -> content_difficulty in [:intermediate, :advanced, :expert]
      
      {:advanced, _} -> content_difficulty in [:advanced, :expert]
      
      _ -> true
    end
  end

  defp prerequisites_met?(prerequisites, skill_levels) do
    Enum.all?(prerequisites, fn {skill_area, required_level} ->
      current_level = Map.get(skill_levels, skill_area, :beginner)
      skill_level_value(current_level) >= skill_level_value(required_level)
    end)
  end

  defp skill_level_value(level) do
    case level do
      :beginner -> 1
      :intermediate -> 2
      :advanced -> 3
      :expert -> 4
    end
  end

  defp order_by_learning_progression(content, skill_levels) do
    # Sort content by learning progression
    content
    |> Enum.sort_by(fn item ->
      {
        get_progression_order(item.skill_area),
        skill_level_value(item.difficulty),
        item.id
      }
    end)
  end

  defp get_progression_order(skill_area) do
    progression_order = %{
      otp_basics: 1,
      gen_server: 2,
      supervisor: 3,
      gen_statem: 4,
      applications: 5,
      testing: 6,
      debugging: 7,
      performance: 8
    }
    
    Map.get(progression_order, skill_area, 99)
  end

  defp apply_preferences(content, preferences) do
    # Apply content type preferences
    content = filter_by_content_type(content, preferences)
    
    # Apply pacing preferences
    content = adjust_for_pacing(content, preferences)
    
    content
  end

  defp filter_by_content_type(content, preferences) do
    content_type_preference = Map.get(preferences, :content_type_preference, :mixed)
    
    case content_type_preference do
      :tutorials_only -> Enum.filter(content, &(&1.type == :tutorial))
      :exercises_only -> Enum.filter(content, &(&1.type == :exercise))
      :mixed -> content
      _ -> content
    end
  end

  defp adjust_for_pacing(content, preferences) do
    pacing = Map.get(preferences, :pacing, :normal)
    
    case pacing do
      :slow -> 
        # Add more foundational content
        add_foundational_content(content)
      
      :fast -> 
        # Remove some intermediate steps
        remove_intermediate_content(content)
      
      :normal -> 
        content
    end
  end

  defp update_skill_levels(skill_levels, content_id, performance_data) do
    # Get content info to determine which skill was practiced
    case OtpSupervisor.Education.ContentManager.get_content(content_id) do
      {:ok, content} ->
        skill_area = content.skill_area
        current_level = Map.get(skill_levels, skill_area, :beginner)
        
        # Update skill level based on performance
        new_level = calculate_new_skill_level(current_level, performance_data)
        
        Map.put(skill_levels, skill_area, new_level)
      
      {:error, _} ->
        skill_levels
    end
  end

  defp calculate_new_skill_level(current_level, performance_data) do
    performance_score = performance_data.score || 0
    
    case {current_level, performance_score} do
      {:beginner, score} when score >= 80 -> :intermediate
      {:intermediate, score} when score >= 85 -> :advanced
      {:advanced, score} when score >= 90 -> :expert
      {level, score} when score < 50 -> downgrade_skill_level(level)
      {level, _} -> level
    end
  end

  defp downgrade_skill_level(level) do
    case level do
      :expert -> :advanced
      :advanced -> :intermediate
      :intermediate -> :beginner
      :beginner -> :beginner
    end
  end

  defp should_regenerate_path?(state, performance_data) do
    # Regenerate path if performance indicates skill level change
    performance_score = performance_data.score || 0
    
    # Regenerate if performance is significantly different from expected
    performance_score < 40 or performance_score > 95
  end

  defp generate_advanced_path(state) do
    # Generate path for advanced content
    case get_advanced_content(state.skill_levels) do
      [] -> {:error, :no_advanced_content}
      content ->
        new_path = %{
          content_sequence: content,
          generated_at: System.monotonic_time(),
          skill_levels: state.skill_levels,
          preferences: state.preferences
        }
        {:ok, new_path}
    end
  end

  defp get_advanced_content(skill_levels) do
    # Get content for advanced learners
    OtpSupervisor.Education.ContentManager.get_advanced_content(skill_levels)
  end

  defp add_foundational_content(content) do
    # Add additional foundational content for slow learners
    content
  end

  defp remove_intermediate_content(content) do
    # Remove some intermediate content for fast learners
    content
  end

  defp via_tuple(student_id) do
    {:via, Registry, {OtpSupervisor.Education.AdaptiveLearningRegistry, student_id}}
  end
end
```

---

## Testing Strategy

### Unit Tests

```elixir
defmodule OtpSupervisor.Education.TutorialSystemTest do
  use ExUnit.Case, async: true
  alias OtpSupervisor.Education.TutorialSystem

  describe "tutorial lifecycle" do
    test "starts tutorial successfully" do
      tutorial_id = "basic_genserver"
      student_id = "student_#{System.unique_integer([:positive])}"
      
      {:ok, _pid} = TutorialSystem.start_link(tutorial_id, student_id)
      
      assert {:ok, response} = TutorialSystem.start_tutorial(tutorial_id, student_id)
      assert response.status == :started
      assert response.step.step_number == 1
    end

    test "advances through tutorial steps" do
      tutorial_id = "basic_genserver"
      student_id = "student_#{System.unique_integer([:positive])}"
      
      {:ok, _pid} = TutorialSystem.start_link(tutorial_id, student_id)
      {:ok, _} = TutorialSystem.start_tutorial(tutorial_id, student_id)
      
      # Submit correct answer for step 1
      correct_answer = %{code: "defmodule MyServer do\n  use GenServer\nend"}
      {:ok, _} = TutorialSystem.submit_step_answer(tutorial_id, student_id, correct_answer)
      
      # Advance to next step
      {:ok, response} = TutorialSystem.next_step(tutorial_id, student_id)
      assert response.status == :step_advanced
      assert response.step.step_number == 2
    end

    test "provides feedback on incorrect answers" do
      tutorial_id = "basic_genserver"
      student_id = "student_#{System.unique_integer([:positive])}"
      
      {:ok, _pid} = TutorialSystem.start_link(tutorial_id, student_id)
      {:ok, _} = TutorialSystem.start_tutorial(tutorial_id, student_id)
      
      # Submit incorrect answer
      incorrect_answer = %{code: "defmodule MyServer do\nend"}
      {:ok, response} = TutorialSystem.submit_step_answer(tutorial_id, student_id, incorrect_answer)
      
      assert response.status == :answer_evaluated
      assert response.evaluation.correct? == false
      assert response.feedback.specific != nil
    end
  end
end
```

### Integration Tests

```elixir
defmodule OtpSupervisor.Education.IntegrationTest do
  use ExUnit.Case, async: false
  
  describe "complete learning flow" do
    test "student completes tutorial and gets exercise recommendation" do
      student_id = "integration_student_#{System.unique_integer([:positive])}"
      tutorial_id = "basic_genserver"
      
      # Start adaptive learning
      {:ok, _} = OtpSupervisor.Education.AdaptiveLearning.start_link(student_id)
      
      # Get recommended content
      {:ok, recommended_content} = OtpSupervisor.Education.AdaptiveLearning.get_next_content(student_id)
      
      # Start recommended tutorial
      {:ok, _} = OtpSupervisor.Education.TutorialSystem.start_link(tutorial_id, student_id)
      {:ok, _} = OtpSupervisor.Education.TutorialSystem.start_tutorial(tutorial_id, student_id)
      
      # Complete tutorial steps
      complete_tutorial_steps(tutorial_id, student_id)
      
      # Update adaptive learning with completion
      performance_data = %{passed?: true, score: 85}
      OtpSupervisor.Education.AdaptiveLearning.update_performance(
        student_id, tutorial_id, performance_data
      )
      
      # Get next recommendation
      {:ok, next_content} = OtpSupervisor.Education.AdaptiveLearning.get_next_content(student_id)
      
      # Should recommend more advanced content
      assert next_content.difficulty in [:intermediate, :advanced]
    end
  end
  
  defp complete_tutorial_steps(tutorial_id, student_id) do
    # Helper to complete all tutorial steps
    # Implementation would depend on specific tutorial structure
  end
end
```

---

## Configuration

### System Configuration

```elixir
# config/config.exs
config :otp_supervisor, :education,
  # Content settings
  content_path: "priv/educational_content",
  max_tutorial_duration: 3_600_000,  # 1 hour
  max_exercise_duration: 1_800_000,  # 30 minutes
  
  # Assessment settings
  auto_assessment_enabled: true,
  pattern_recognition_enabled: true,
  code_quality_analysis: true,
  
  # Adaptive learning settings
  adaptive_learning_enabled: true,
  skill_level_update_threshold: 0.1,
  learning_path_regeneration_interval: 86_400_000,  # 24 hours
  
  # Gamification settings
  achievements_enabled: true,
  progress_tracking_enabled: true,
  leaderboards_enabled: false,
  
  # Analytics settings
  analytics_enabled: true,
  performance_tracking: true,
  learning_analytics: true,
  
  # Sandbox settings
  tutorial_sandbox_timeout: 1_800_000,  # 30 minutes
  exercise_sandbox_timeout: 3_600_000,  # 1 hour
  max_concurrent_sandboxes: 100
```

---

## Conclusion

This Educational Framework design provides a comprehensive foundation for interactive OTP learning. The system emphasizes:

- **Interactive Learning**: Hands-on experience with real OTP systems
- **Adaptive Content**: Personalized learning paths based on individual progress
- **Comprehensive Assessment**: Automated evaluation with detailed feedback
- **Pattern Recognition**: Code analysis for targeted improvement suggestions
- **Gamification**: Engagement through achievements and progress tracking
- **Scalability**: Support for multiple concurrent learning sessions

The framework integrates seamlessly with the existing sandbox architecture while providing the educational capabilities needed to transform the platform into a comprehensive OTP learning environment.

Next steps include implementing the remaining technical design documents and beginning development of the educational components.