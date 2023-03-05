const d3 = require('d3');


function draw(d3n) {
  var svg = d3n.createSVG(800, 200);

  svg.append("rect")
    .attr("width", "100%")
    .attr("height", "100%")
    .style("fill", "#f9f0de");

  var x = d3.scaleLinear()
    .domain([0, 100])
    .range([0, 400]);

  svg.call(d3.axisBottom(x));

  svg.append("circle")
    .attr("cx", x(10)).attr("cy", 100).attr("r", 30).style("fill", "#005aa7");
  svg.append("circle")
    .attr("cx", x(50)).attr("cy", 100).attr("r", 40).style("fill", "#5ec5ee");
  svg.append("circle")
    .attr("cx", x(90)).attr("cy", 100).attr("r", 50).style("fill", "#e6007b");
}

exports.draw = draw;
