var Plot = Backbone.View.extend({
    tagName: "canvas",
    ctx: null,
    chart: null,

    initialize: function() {
        
        this.ctx = this.$el.get(0).getContext("2d");
        //this.chart = new Chart(this.ctx).Line(window.plots.get('hamringsmomentum_total'));
        
        window.plots.on('change', _.bind(this.render, this));
        
        this.$el.css({
            width: "100%",
            height: "100%"
        });
    },
    
    render: function() {
        this.chart = new Chart(this.ctx).Line({
            datasets: [{
                fillColor: "rgba(220,220,220,0.2)",
                strokeColor: "rgba(220,220,220,1)",
                pointColor: "rgba(220,220,220,1)",
                pointStrokeColor: "#fff",
                pointHighlightFill: "#fff",
                pointHighlightStroke: "rgba(220,220,220,1)",
                label: "Hamringsmomentum totalt",
                data: window.plots.get('hamringsmomentum_total')
            }]
        });
        this.chart.update();
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
