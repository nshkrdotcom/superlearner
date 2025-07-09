const ChartWidget = {
  mounted() {
    this.initChart();
  },
  
  updated() {
    this.initChart();
  },
  
  initChart() {
    const chartType = this.el.dataset.chartType;
    const chartData = JSON.parse(this.el.dataset.chartData);
    const chartConfig = JSON.parse(this.el.dataset.chartConfig);
    
    // Clear existing chart
    this.el.innerHTML = '';
    
    // Create SVG element
    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    svg.setAttribute('viewBox', '0 0 400 300');
    svg.setAttribute('class', 'w-full h-full');
    
    // Add grid if enabled
    if (chartConfig.show_grid) {
      const defs = document.createElementNS('http://www.w3.org/2000/svg', 'defs');
      const pattern = document.createElementNS('http://www.w3.org/2000/svg', 'pattern');
      pattern.setAttribute('id', `grid-${this.el.id}`);
      pattern.setAttribute('width', '40');
      pattern.setAttribute('height', '30');
      pattern.setAttribute('patternUnits', 'userSpaceOnUse');
      
      const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
      path.setAttribute('d', 'M 40 0 L 0 0 0 30');
      path.setAttribute('fill', 'none');
      path.setAttribute('stroke', '#10b981');
      path.setAttribute('stroke-width', '0.5');
      path.setAttribute('opacity', '0.2');
      
      pattern.appendChild(path);
      defs.appendChild(pattern);
      svg.appendChild(defs);
      
      const gridRect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
      gridRect.setAttribute('width', '100%');
      gridRect.setAttribute('height', '100%');
      gridRect.setAttribute('fill', `url(#grid-${this.el.id})`);
      svg.appendChild(gridRect);
    }
    
    // Render chart based on type
    switch (chartType) {
      case 'line':
        this.renderLineChart(svg, chartData, chartConfig);
        break;
      case 'bar':
        this.renderBarChart(svg, chartData, chartConfig);
        break;
      case 'pie':
        this.renderPieChart(svg, chartData, chartConfig);
        break;
      case 'area':
        this.renderAreaChart(svg, chartData, chartConfig);
        break;
    }
    
    this.el.appendChild(svg);
  },
  
  renderLineChart(svg, data, config) {
    if (data.length === 0) return;
    
    const points = data.map((point, index) => {
      const x = this.pointX(index, data.length);
      const y = this.pointY(point, data);
      return `${x},${y}`;
    }).join(' ');
    
    if (data.length > 1) {
      const polyline = document.createElementNS('http://www.w3.org/2000/svg', 'polyline');
      polyline.setAttribute('points', points);
      polyline.setAttribute('fill', 'none');
      polyline.setAttribute('stroke', this.getColor(config.color_scheme, 0));
      polyline.setAttribute('stroke-width', '2');
      if (config.animated) {
        polyline.setAttribute('class', 'animate-pulse');
      }
      svg.appendChild(polyline);
    }
    
    // Add data points
    data.forEach((point, index) => {
      const circle = document.createElementNS('http://www.w3.org/2000/svg', 'circle');
      circle.setAttribute('cx', this.pointX(index, data.length));
      circle.setAttribute('cy', this.pointY(point, data));
      circle.setAttribute('r', '3');
      circle.setAttribute('fill', this.getColor(config.color_scheme, 0));
      circle.setAttribute('class', 'hover:fill-green-300 transition-colors cursor-pointer');
      svg.appendChild(circle);
    });
  },
  
  renderBarChart(svg, data, config) {
    if (data.length === 0) return;
    
    data.forEach((point, index) => {
      const rect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
      rect.setAttribute('x', this.barX(index, data.length));
      rect.setAttribute('y', this.barY(point, data));
      rect.setAttribute('width', this.barWidth(data.length));
      rect.setAttribute('height', this.barHeight(point, data));
      rect.setAttribute('fill', this.getColor(config.color_scheme, 0));
      rect.setAttribute('class', 'hover:fill-green-300 transition-colors cursor-pointer');
      svg.appendChild(rect);
    });
  },
  
  renderPieChart(svg, data, config) {
    if (data.length === 0) return;
    
    const total = data.reduce((sum, point) => sum + this.getPointValue(point), 0);
    let startAngle = 0;
    
    data.forEach((point, index) => {
      const value = this.getPointValue(point);
      const angle = (value / total) * 360;
      
      const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
      path.setAttribute('d', this.pieSlicePath(startAngle, angle));
      path.setAttribute('fill', this.getColor(config.color_scheme, index));
      path.setAttribute('class', 'hover:opacity-80 transition-opacity cursor-pointer');
      path.setAttribute('transform', 'translate(200, 150)');
      svg.appendChild(path);
      
      startAngle += angle;
    });
  },
  
  renderAreaChart(svg, data, config) {
    if (data.length === 0) return;
    
    const points = data.map((point, index) => {
      const x = this.pointX(index, data.length);
      const y = this.pointY(point, data);
      return `${x},${y}`;
    }).join(' ');
    
    if (data.length > 1) {
      const firstX = this.pointX(0, data.length);
      const lastX = this.pointX(data.length - 1, data.length);
      const areaPoints = `${firstX},300 ${points} ${lastX},300`;
      
      const polygon = document.createElementNS('http://www.w3.org/2000/svg', 'polygon');
      polygon.setAttribute('points', areaPoints);
      polygon.setAttribute('fill', 'rgba(16, 185, 129, 0.3)');
      polygon.setAttribute('stroke', this.getColor(config.color_scheme, 0));
      polygon.setAttribute('stroke-width', '2');
      svg.appendChild(polygon);
    }
  },
  
  getPointValue(point) {
    return point.value || point.y || point.cpu || point.memory || 0;
  },
  
  getColor(scheme, index) {
    const colors = {
      green: ['#10b981'],
      blue: ['#3b82f6'],
      multi: ['#10b981', '#3b82f6', '#f59e0b', '#ef4444', '#8b5cf6']
    };
    const colorArray = colors[scheme] || colors.green;
    return colorArray[index % colorArray.length];
  },
  
  pointX(index, total) {
    return (index / (total - 1)) * 360 + 20;
  },
  
  pointY(point, data) {
    const value = this.getPointValue(point);
    const maxVal = Math.max(...data.map(p => this.getPointValue(p)));
    const minVal = Math.min(...data.map(p => this.getPointValue(p)));
    
    if (maxVal === minVal) {
      return 150;
    }
    
    return 280 - ((value - minVal) / (maxVal - minVal)) * 260;
  },
  
  barX(index, total) {
    const barWidth = 360 / total;
    return index * barWidth + 20;
  },
  
  barY(point, data) {
    return this.pointY(point, data);
  },
  
  barWidth(total) {
    return Math.max(360 / total - 2, 1);
  },
  
  barHeight(point, data) {
    const value = this.getPointValue(point);
    const maxVal = Math.max(...data.map(p => this.getPointValue(p)));
    const minVal = Math.min(...data.map(p => this.getPointValue(p)));
    
    if (maxVal === minVal) {
      return 1;
    }
    
    return ((value - minVal) / (maxVal - minVal)) * 260;
  },
  
  pieSlicePath(startAngle, angle) {
    const startRadians = (startAngle * Math.PI) / 180;
    const endRadians = ((startAngle + angle) * Math.PI) / 180;
    
    const x1 = 80 * Math.cos(startRadians);
    const y1 = 80 * Math.sin(startRadians);
    const x2 = 80 * Math.cos(endRadians);
    const y2 = 80 * Math.sin(endRadians);
    
    const largeArc = angle > 180 ? 1 : 0;
    
    return `M 0,0 L ${x1},${y1} A 80,80 0 ${largeArc},1 ${x2},${y2} Z`;
  }
};

export default { ChartWidget };