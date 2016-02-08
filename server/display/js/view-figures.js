var Plot = Backbone.View.extend({
    tagName: "canvas",
    ctx: null,
    chart: null,
    options: {
	animation: false,
	omitXLabels: true
    },

    initialize: function() {
        
        this.ctx = this.$el.get(0).getContext("2d");
        //this.chart = new Chart(this.ctx).Line(window.plots.get('hamringsmomentum_total'));
        
        window.plots.on('change', _.bind(this.render, this));

	this.$el.css({'width': '100%', 'height': '100%'});

    },

    drawGraph: function() {
	this.chart = new Chart(this.ctx).Line({
	    labels: Array.apply(null, Array(window.plots.get('hamringsmomentum_total').length)).map(String.prototype.valueOf, ""), // create an array of "" strings
	    datasets: [
		{
		    label: "Hamringsmomentum totalt",
		    fillColor: "rgba(255,255,255,0)",
                    strokeColor: "rgba(0,0,0,1)",
		    pointStrokeColor: "rgba(255,255,255,0)",
                    data: window.plots.get('hamringsmomentum_total')
		}
	    ]
        }, this.options);

    },
    
    render: function() {
	/*
	this.drawGraph();
        this.chart.update();
	*/
    }
});

var PlotsView = Backbone.View.extend({
    id: "plots",
    

    events: {
        
    },
    
    initialize: function() {
        console.log('plots view initialized');
        
        var plot = new Plot();
        this.$el.append(plot.$el);

        // setInterval(_.bind(this.advance, this), 18000);
    },

    render: function() {
        console.log('plots view render');
    }
    
});
