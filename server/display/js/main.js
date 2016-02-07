window.state = {};

$(function() {
    var scores = new Backbone.Model();
    var jerseys = new Backbone.Model();
    var momentum = new Backbone.Model();
    var plots = new Backbone.Model();

    var socket = io.connect();
    
    var compositeModel = new Backbone.Model();
    compositeModel.set({
        'scores': scores,
        'jerseys': jerseys,
        'momentum': momentum,
        'plots': plots
    } );

    window.state = {"scores": scores, "momentum": momentum, "jerseys": jerseys};
    window.plots = plots;
    
    var scoreboardview = new Scoreboard({ el: document.getElementById("scoreboard"), model: compositeModel});
    
    var loggerview = new Logger({model: scores});
    $('#logContainer').append(loggerview.$el);
    
    var plotsview = new PlotsView({el: document.getElementById('plots')});

    socket.on('state', function (data) {
	// console.log(['state', data]);
	
        scores.set(data.cur_scores);
        momentum.set(data.momentum);
        jerseys.set(data.jerseys);
    });
    
    // Take care of plots
    socket.on('plots', function(data) {
	// console.log(['plots', data]);
        plots.set(data);

	
    });

    
    // We're updating the yellow jersey more frequently than the others
    jerseys.on('change', function() {
        $('.green').removeClass('green');
        $('.dotted').removeClass('dotted');

        $("#score-" + jerseys.get('green')).addClass('green');
        $("#score-" + jerseys.get('dotted')).addClass('dotted');
    });

    socket.on('reload', function(data) {
        window.location.reload();
    });

    // Reload the iframe automatically
    setInterval(function() {
        $('iframe').attr('src', $('iframe').attr('src'));
    }, 120000);

});
