// Three.js Cluster Visualization
let scene, camera, renderer, raycaster, mouse;
let nodeGroups = [];
let interactableObjects = [];
let hoveredObject = null;

function initThreeJS() {
  const container = document.getElementById('cluster-3d-viz');
  if (!container) return;
  
  const width = container.offsetWidth;
  const height = 500;
  
  scene = new THREE.Scene();
  scene.background = new THREE.Color(0x111827);
  
  camera = new THREE.PerspectiveCamera(75, width / height, 0.1, 1000);
  camera.position.set(0, 10, 20);
  
  renderer = new THREE.WebGLRenderer({antialias: true});
  renderer.setSize(width, height);
  
  // Initialize raycaster and mouse for interaction
  raycaster = new THREE.Raycaster();
  mouse = new THREE.Vector2();
  
  const existingCanvas = container.querySelector('canvas');
  if (existingCanvas) existingCanvas.remove();
  container.appendChild(renderer.domElement);
  
  const ambientLight = new THREE.AmbientLight(0x404040, 0.6);
  scene.add(ambientLight);
  
  const directionalLight = new THREE.DirectionalLight(0x10b981, 0.8);
  directionalLight.position.set(10, 10, 5);
  scene.add(directionalLight);
  
  setupControls();
  setupInteraction();
  animate();
}

function setupControls() {
  let isMouseDown = false;
  let mouseX = 0, mouseY = 0;
  let cameraDistance = 20;
  let cameraAngleX = 0.3;
  let cameraAngleY = 0;
  
  const canvas = renderer.domElement;
  
  function updateCameraPosition() {
    camera.position.x = Math.sin(cameraAngleY) * Math.cos(cameraAngleX) * cameraDistance;
    camera.position.y = Math.sin(cameraAngleX) * cameraDistance;
    camera.position.z = Math.cos(cameraAngleY) * Math.cos(cameraAngleX) * cameraDistance;
    camera.lookAt(0, 0, 0);
  }
  
  updateCameraPosition();
  
  canvas.addEventListener('mousedown', (event) => {
    isMouseDown = true;
    mouseX = event.clientX;
    mouseY = event.clientY;
    canvas.style.cursor = 'grabbing';
  });
  
  canvas.addEventListener('mousemove', (event) => {
    if (!isMouseDown) return;
    
    const deltaX = event.clientX - mouseX;
    const deltaY = event.clientY - mouseY;
    
    cameraAngleY -= deltaX * 0.01;
    cameraAngleX += deltaY * 0.01;
    cameraAngleX = Math.max(-Math.PI/2 + 0.1, Math.min(Math.PI/2 - 0.1, cameraAngleX));
    
    updateCameraPosition();
    
    mouseX = event.clientX;
    mouseY = event.clientY;
  });
  
  canvas.addEventListener('mouseup', () => {
    isMouseDown = false;
    canvas.style.cursor = 'grab';
  });
  
  canvas.addEventListener('wheel', (event) => {
    event.preventDefault();
    cameraDistance += event.deltaY * 0.1;
    cameraDistance = Math.max(3, Math.min(100, cameraDistance));
    updateCameraPosition();
  });
  
  canvas.style.cursor = 'grab';
}

function setupInteraction() {
  const canvas = renderer.domElement;
  
  // Mouse move for hover detection
  canvas.addEventListener('mousemove', onMouseMove, false);
  canvas.addEventListener('click', onMouseClick, false);
  
  // Close button for details panel
  const closeButton = document.getElementById('close-details');
  if (closeButton) {
    closeButton.addEventListener('click', hideDetailsPanel);
  }
}

function onMouseMove(event) {
  if (!renderer || !camera) return;
  
  const rect = renderer.domElement.getBoundingClientRect();
  mouse.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
  mouse.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;
  
  raycaster.setFromCamera(mouse, camera);
  const intersects = raycaster.intersectObjects(interactableObjects);
  
  if (intersects.length > 0) {
    const object = intersects[0].object;
    if (hoveredObject !== object) {
      if (hoveredObject) {
        // Reset previous hovered object
        hoveredObject.scale.set(1, 1, 1);
      }
      
      hoveredObject = object;
      // Scale up hovered object
      hoveredObject.scale.set(2, 2, 2);
      
      // Show details panel
      if (object.userData) {
        showDetailsPanel(object.userData);
      }
      
      renderer.domElement.style.cursor = 'pointer';
    }
  } else {
    if (hoveredObject) {
      hoveredObject.scale.set(1, 1, 1);
      hoveredObject = null;
      renderer.domElement.style.cursor = 'grab';
    }
  }
}

function onMouseClick(event) {
  if (!renderer || !camera) return;
  
  const rect = renderer.domElement.getBoundingClientRect();
  mouse.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
  mouse.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;
  
  raycaster.setFromCamera(mouse, camera);
  const intersects = raycaster.intersectObjects(interactableObjects);
  
  if (intersects.length > 0) {
    const object = intersects[0].object;
    if (object.userData) {
      showDetailsPanel(object.userData, true); // true for clicked/pinned
    }
  }
}

function renderClusterVisualization3D() {
  try {
    const clusterDataEl = document.getElementById('cluster-data-json');
    if (!clusterDataEl) return;
    
    const clusterData = JSON.parse(clusterDataEl.textContent || '{}');
    
    if (!scene) initThreeJS();
    
    nodeGroups.forEach(group => scene.remove(group));
    nodeGroups = [];
    interactableObjects = []; // Clear interactable objects for fresh render
    
    const nodes = Object.entries(clusterData);
    if (nodes.length === 0) return;
    
    nodes.forEach(([nodeName, nodeData], index) => {
      const nodeGroup = new THREE.Group();
      
      const angle = (index / nodes.length) * Math.PI * 2;
      const radius = Math.max(6, nodes.length * 1.5); // Much closer together
      nodeGroup.position.x = Math.cos(angle) * radius;
      nodeGroup.position.z = Math.sin(angle) * radius;
      
      if (nodeData.children) {
        renderNode3D(nodeGroup, nodeData, 0, 0, 0, 0);
      }
      
      scene.add(nodeGroup);
      nodeGroups.push(nodeGroup);
    });
    
    console.log('3D Cluster visualization rendered successfully');
    
  } catch (error) {
    console.error('Error rendering 3D cluster visualization:', error);
  }
}

function renderNode3D(parent, nodeData, x, y, z, level) {
  const size = getNodeSize(nodeData.type);
  const geometry = new THREE.SphereGeometry(size, 16, 16);
  const material = new THREE.MeshPhongMaterial({ 
    color: getNodeColor(nodeData.type, nodeData.alive),
    shininess: 80
  });
  
  const sphere = new THREE.Mesh(geometry, material);
  sphere.position.set(x, y, z);
  
  // Store node data for interaction
  sphere.userData = {
    name: nodeData.name || 'Unknown',
    pid: nodeData.pid,
    type: nodeData.type,
    behavior: nodeData.behavior,
    alive: nodeData.alive,
    application: nodeData.application,
    memory: nodeData.memory,
    message_queue_len: nodeData.message_queue_len,
    strategy: nodeData.strategy,
    child_count: nodeData.child_count,
    restart_intensity: nodeData.restart_intensity,
    restart_period: nodeData.restart_period,
    process_count: nodeData.process_count,
    memory_usage: nodeData.memory_usage,
    links: nodeData.links,
    monitors: nodeData.monitors,
    level: level
  };
  
  // Add to interactable objects for mouse detection
  interactableObjects.push(sphere);
  
  parent.add(sphere);
  
  if (nodeData.children && nodeData.children.length > 0) {
    // Keep child radius small and reasonable
    const childRadius = Math.min(3, Math.max(1.5, nodeData.children.length * 0.3));
    const childY = y - 2.5; // Smaller vertical spacing
    
    nodeData.children.forEach((child, index) => {
      const childAngle = (index / nodeData.children.length) * Math.PI * 2;
      const childX = x + Math.cos(childAngle) * childRadius;
      const childZ = z + Math.sin(childAngle) * childRadius;
      
      // Only draw line if the distance is reasonable
      const distance = Math.sqrt(
        Math.pow(childX - x, 2) + 
        Math.pow(childY - y, 2) + 
        Math.pow(childZ - z, 2)
      );
      
      if (distance < 10) { // Only draw lines for nearby children
        const lineGeometry = new THREE.BufferGeometry().setFromPoints([
          new THREE.Vector3(x, y, z),
          new THREE.Vector3(childX, childY, childZ)
        ]);
        const lineMaterial = new THREE.LineBasicMaterial({ 
          color: 0x10b981, 
          opacity: 0.4, 
          transparent: true 
        });
        const line = new THREE.Line(lineGeometry, lineMaterial);
        parent.add(line);
      }
      
      // Limit recursion depth to prevent excessive nesting
      if (level < 4) {
        renderNode3D(parent, child, childX, childY, childZ, level + 1);
      }
    });
  }
}

function getNodeSize(type) {
  switch(type) {
    case 'cluster_node': return 0.05;  // 95% smaller
    case 'application': return 0.035;  // 95% smaller
    case 'supervisor': return 0.025;   // 95% smaller
    default: return 0.015;             // 95% smaller
  }
}

function getNodeColor(type, alive) {
  if (alive === false) return 0xef4444;
  
  switch(type) {
    case 'cluster_node': return 0x10b981;
    case 'application': return 0xf59e0b;
    case 'supervisor': return 0x3b82f6;
    default: return 0x86efac;
  }
}

function showDetailsPanel(nodeData, pinned = false) {
  const panel = document.getElementById('node-details-panel');
  const title = document.getElementById('details-title');
  const content = document.getElementById('details-content');
  
  if (!panel || !title || !content) return;
  
  // Set title based on node type
  const nodeType = formatNodeType(nodeData.type || 'unknown');
  title.textContent = nodeType;
  
  // Build content HTML using string concatenation to avoid template literals
  let html = '';
  
  // Basic info
  html += '<div class="border-b border-green-500/20 pb-2 mb-2">';
  html += '<div class="font-bold text-green-300">' + (nodeData.name || 'Unknown') + '</div>';
  if (nodeData.pid) {
    html += '<div class="text-xs text-green-400/70">PID: ' + nodeData.pid + '</div>';
  }
  html += '</div>';
  
  // Status and details
  html += '<div class="space-y-1">';
  
  // Status
  const statusColor = nodeData.alive !== false ? '#10b981' : '#ef4444';
  const statusText = nodeData.alive !== false ? '● Alive' : '● Dead';
  html += '<div class="flex justify-between"><span>Status:</span><span style="color: ' + statusColor + ';">' + statusText + '</span></div>';
  
  // Type and behavior
  if (nodeData.behavior && nodeData.behavior !== 'unknown') {
    html += '<div class="flex justify-between"><span>Behavior:</span><span>' + formatBehavior(nodeData.behavior) + '</span></div>';
  }
  
  // Application
  if (nodeData.application && nodeData.application !== 'unknown') {
    html += '<div class="flex justify-between"><span>Application:</span><span>' + nodeData.application + '</span></div>';
  }
  
  // Memory
  if (nodeData.memory && nodeData.memory > 0) {
    html += '<div class="flex justify-between"><span>Memory:</span><span>' + formatBytes(nodeData.memory) + '</span></div>';
  }
  
  // Message queue
  if (nodeData.message_queue_len && nodeData.message_queue_len > 0) {
    html += '<div class="flex justify-between"><span>Queue:</span><span>' + nodeData.message_queue_len + ' msgs</span></div>';
  }
  
  // Supervisor specific info
  if (nodeData.type === 'supervisor') {
    if (nodeData.strategy) {
      html += '<div class="flex justify-between"><span>Strategy:</span><span>' + nodeData.strategy + '</span></div>';
    }
    if (nodeData.child_count !== undefined) {
      html += '<div class="flex justify-between"><span>Children:</span><span>' + nodeData.child_count + '</span></div>';
    }
    if (nodeData.restart_intensity !== undefined) {
      html += '<div class="flex justify-between"><span>Restart:</span><span>' + nodeData.restart_intensity + '/' + (nodeData.restart_period || 5) + 's</span></div>';
    }
  }
  
  // Application specific info
  if (nodeData.type === 'application') {
    if (nodeData.process_count) {
      html += '<div class="flex justify-between"><span>Processes:</span><span>' + nodeData.process_count + '</span></div>';
    }
    if (nodeData.memory_usage && nodeData.memory_usage > 0) {
      html += '<div class="flex justify-between"><span>Total Memory:</span><span>' + formatBytes(nodeData.memory_usage) + '</span></div>';
    }
  }
  
  // Links and monitors
  if (nodeData.links && nodeData.links.length > 0) {
    html += '<div class="flex justify-between"><span>Links:</span><span>' + nodeData.links.length + '</span></div>';
  }
  if (nodeData.monitors && nodeData.monitors.length > 0) {
    html += '<div class="flex justify-between"><span>Monitors:</span><span>' + nodeData.monitors.length + '</span></div>';
  }
  
  html += '</div>';
  
  content.innerHTML = html;
  panel.classList.remove('hidden');
  
  // If pinned (clicked), add a visual indicator
  if (pinned) {
    panel.style.borderColor = 'rgba(16, 185, 129, 0.8)';
    panel.style.backgroundColor = 'rgba(17, 24, 39, 0.98)';
  } else {
    panel.style.borderColor = 'rgba(16, 185, 129, 0.3)';
    panel.style.backgroundColor = 'rgba(17, 24, 39, 0.95)';
  }
}

function hideDetailsPanel() {
  const panel = document.getElementById('node-details-panel');
  if (panel) {
    panel.classList.add('hidden');
  }
}

function formatNodeType(type) {
  switch(type) {
    case 'cluster_node': return 'Cluster Node';
    case 'application': return 'Application';
    case 'supervisor': return 'Supervisor';
    case 'worker': return 'Worker Process';
    case 'virtual_supervisor': return 'Virtual Supervisor';
    default: return type ? type.charAt(0).toUpperCase() + type.slice(1) : 'Unknown';
  }
}

function formatBehavior(behavior) {
  switch(behavior) {
    case 'gen_server': return 'GenServer';
    case 'gen_statem': return 'GenStateMachine';
    case 'gen_event': return 'GenEvent';
    case 'supervisor': return 'Supervisor';
    case 'task': return 'Task';
    case 'agent': return 'Agent';
    default: return behavior ? behavior.charAt(0).toUpperCase() + behavior.slice(1) : 'Unknown';
  }
}

function formatBytes(bytes) {
  if (bytes === 0) return '0 Bytes';
  const k = 1024;
  const sizes = ['Bytes', 'KB', 'MB', 'GB'];
  const i = Math.floor(Math.log(bytes) / Math.log(k));
  return parseFloat((bytes / Math.pow(k, i)).toFixed(2)) + ' ' + sizes[i];
}

function animate() {
  requestAnimationFrame(animate);
  renderer.render(scene, camera);
}

// Initialize
let renderTimeout;
function debouncedRender3D() {
  clearTimeout(renderTimeout);
  renderTimeout = setTimeout(renderClusterVisualization3D, 100);
}

if (!window.cluster3DVizListenersSet) {
  window.addEventListener('phx:page-loading-stop', debouncedRender3D);
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', debouncedRender3D);
  } else {
    debouncedRender3D();
  }
  window.cluster3DVizListenersSet = true;
}

debouncedRender3D();