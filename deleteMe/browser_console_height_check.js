// Run this in your browser console on each page to get actual rendered heights

(() => {
  console.log('🔍 Getting ACTUAL rendered header heights...');
  
  // Find the header element
  const headers = document.querySelectorAll('div[class*="h-1"]');
  let targetHeader = null;
  
  for (const header of headers) {
    if (header.className.includes('flex') && 
        header.className.includes('items-center') && 
        header.className.includes('border-b')) {
      targetHeader = header;
      break;
    }
  }
  
  if (!targetHeader) {
    console.error('❌ Header not found!');
    return;
  }
  
  const rect = targetHeader.getBoundingClientRect();
  const computed = window.getComputedStyle(targetHeader);
  
  console.log('📊 Header measurements:');
  console.log(`   🎯 Actual height (getBoundingClientRect): ${rect.height}px`);
  console.log(`   📐 Computed height: ${computed.height}`);
  console.log(`   📏 offsetHeight: ${targetHeader.offsetHeight}px`);
  console.log(`   📋 clientHeight: ${targetHeader.clientHeight}px`);
  console.log(`   🏷️  CSS classes: ${targetHeader.className}`);
  console.log(`   📍 Position: top=${rect.top}, left=${rect.left}`);
  
  // Highlight the header element
  targetHeader.style.border = '3px solid red';
  targetHeader.style.backgroundColor = 'rgba(255, 0, 0, 0.1)';
  
  console.log('📍 Header element highlighted in red');
  
  return {
    actualHeight: rect.height,
    computedHeight: computed.height,
    offsetHeight: targetHeader.offsetHeight,
    clientHeight: targetHeader.clientHeight,
    className: targetHeader.className,
    position: { top: rect.top, left: rect.left, right: rect.right, bottom: rect.bottom }
  };
})();