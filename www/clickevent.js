

$(document).ready(function(){
// boiler plate postMessage plotly code (https://github.com/plotly/postMessage-API)
var plot = document.getElementById('planet_plot').contentWindow;

pinger = setInterval(function(){
    plot.postMessage({task: 'ping'}, 'https://plot.ly');
}, 100);

var clickResponse = function(e) {
     plot = document.getElementById('planet_plot').contentWindow;
    var message = e.data;
     console.log( 'New message from chart', message );
    if(message.pong) {
        // tell the embedded plot that you want to listen to click events
        clearInterval(pinger);
        plot.postMessage({
              task: 'listen', events: ['click']}, 'https://plot.ly');
          plot.postMessage({
            task: 'relayout',
            'update': {hovermode: 'closest'},
        },
        'https://plot.ly');
    }
    else if(message.type === 'click') {
        var curveNumber = message['points'][0]['curveNumber'],
            pointNumber = message['points'][0]['pointNumber'];

        var link;
        var traces = message.points[0].data;
        if(traces !== null && typeof traces === 'object') {
            link = traces.urls[pointNumber];
        } else {
            link = traces[curveNumber].urls[pointNumber];
        }

        console.log(link);

        var win = window.open(link, '_blank');
        win.focus();
    }
};

window.addEventListener("message", clickResponse, false);

});