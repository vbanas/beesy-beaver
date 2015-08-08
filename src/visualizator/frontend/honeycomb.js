
//svg sizes and margins
var margin = {
    top: 30,
    right: 20,
    bottom: 20,
    left: 50
};

var width = $(window).width() - margin.left - margin.right - 40;
var height = $(window).height() - margin.top - margin.bottom - 80;

//Calculate the center positions of each hexagon	
// var points = [];
// for (var i = 0; i < MapRows; i++) {
//     for (var j = 0; j < MapColumns; j++) {
//         points.push([hexRadius * j * 1.75, hexRadius * i * 1.5]);
//     }//for j
// }//for i

function draw_honeycomb (columns, rows, points, pivot, score) {
    //The maximum radius the hexagons can have to still fit the screen
    var hexRadius = d3.min([width/((columns + 0.5) * Math.sqrt(3)),
			    height/((rows + 1/3) * 1.5)]);

    //Set the hexagon radius
    var hexbin = d3.hexbin().radius(hexRadius);

    d3.select ("svg").remove ();
    d3.select ("#score-value").text (score);

    var coords = [];
    for (var i = 0; i < rows; i++) {
        for (var j = 0; j < columns; j++) {
            coords.push([hexRadius * j * 1.75, hexRadius * i * 1.5]);
        }//for j
    }//for i

    var hexbins = hexbin (coords);

    //Create SVG element
    var svg = d3.select("#chart").append("svg")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
        .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    //Start drawing the hexagons
    var hexagons =
        svg.append("g")
        .selectAll(".hexagon")
        .data(hexbins)
        .enter()
        .append("path")
        .attr("class", "hexagon")
        .attr("d", function (d) {
	    return "M" + d.x + "," + d.y + hexbin.hexagon();
	})
        .attr("stroke", function (d,i) {
	    return "#fff";
	})
        .attr("stroke-width", "1px")
        .style("fill", function (d,i) {
            if (points [i] == 1) {
                return "#FFC255";
            } else {
                return "#EAEAEB";
            }
        });

    var hexbin_element = hexbins[pivot.ROW * columns + pivot.COL];

    svg.append ("circle")
        .attr("cx", hexbin_element.x)
        .attr("cy", hexbin_element.y)
        .attr("r", "5")
        .style("fill", "#abcdef");
}
