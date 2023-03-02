const d3 = require('d3');


function drawOne(d3n) {
  var svg = d3n.createSVG(800, 200);

  var x = d3.scaleLinear()
    .domain([0, 100])
    .range([0, 400]);

  svg.call(d3.axisBottom(x));

  svg.append("circle")
    .attr("cx", x(10)).attr("cy", 100).attr("r", 30).style("fill", "blue");
  svg.append("circle")
    .attr("cx", x(50)).attr("cy", 100).attr("r", 40).style("fill", "blue");
  svg.append("circle")
    .attr("cx", x(90)).attr("cy", 100).attr("r", 50).style("fill", "blue");
}

exports.drawOne = drawOne;
