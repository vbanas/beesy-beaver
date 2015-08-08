

var sleep_time = 100;


//util functions
function update_data(data) {
    draw_honeycomb(data.COLUMNS,
                   data.ROWS,
                   data.POINTS,
                   data.PIVOT,
                   data.SCORE,
                   data.UNITS_LEFT);
}


function send_command (command, callback) {
    $.get("/send-command?command=" + command, callback);
}


function decode_command (command) {
    var west_commands = ["p", "'", "!", ".", "0", "3"];
    var east_commands = ["b", "c", "e", "f", "y", "2"];
    var south_west_commands = ["a", "g", "h", "i", "j", "4"];
    var south_east_commands = ["l", "m", "n", "o", " ", "5"];
    var clockwise_commands = ["d", "q", "r", "v", "z", "1"];
    var counter_clockwise_commands = ["k", "s", "t", "u", "w", "x"];

    if (west_commands.indexOf (command) != -1)
        return "west";
    else if (east_commands.indexOf (command) != -1)
        return "east";
    else if (south_east_commands.indexOf (command) != -1)
        return "south-east";
    else if (south_west_commands.indexOf (command) != -1)
        return "south-west";
    else if (clockwise_commands.indexOf (command) != -1)
        return "clockwise";
    else if (counter_clockwise_commands.indexOf (command) != -1)
        return "counter-clockwise";
    else
        return "";
}


//events
d3.select("#get-current-map")
    .on("click", function () {
        $.get("/get-map", update_data)});



d3.select("#send-command")
    .on("click", function() {
        var commands = d3.select("#command-value").property("value");

        var command_index = 0;
        
        var send_command_on_timer = function () {
            if (command_index < commands.length) {
                console.log ("sending command index = " + command_index + "; command = " + decode_command (commands [command_index]));
                
                send_command( decode_command (commands [command_index]),
                              function (data) {
                                  update_data (data);

                                  command_index++;
                                  if (data.IS_TERMINATED == 0)
                                      setTimeout (send_command_on_timer, sleep_time);
                                  else
                                      console.log ("terminated");
                              });
            }
        };
        send_command_on_timer ();
    });


d3.select("body").on("keydown", function () {
    var code = d3.event.keyCode;
    var command;
    // a
    if (code == 65) {
        command = "west";
    }
    // d
    else if (code == 68) {
        command = "east";
    }
    // z
    else if (code == 90) {
        command = "south-west";
    }
    // x
    else if (code == 88) {
        command = "south-east";
    }
    // q
    else if (code == 81) {
        command = "counter-clockwise";
    }
    // e
    else if (code == 69) {
        command = "clockwise";
    }

    if (command !== undefined)
        send_command (command, update_data);
});