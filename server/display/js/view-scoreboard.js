var Scoreboard = Backbone.View.extend({
    tagName: "section",
    id: "scoreboard",
    children: [],

    initialize: function(arg1, arg2) {
        this.model.on('change', _.bind(this.render, this));
        
        // Create all the kitchen widgets
        var kitchens = [
            "GL1", 
            "GL2", "ML2", "NY2",
            "GL3", "ML3", "NY3",
            "GL4", "ML4", "NY4",
            "GL5", "ML5", "NY5",
            "GL6", "ML6", "NY6",
            "GL7", "ML7", "NY7",
            "GL8", "ML8", "NY8"
        ];

        for (var kitchen in kitchens) {
            var unit = new ScoreboardUnit({
                model: this.model, 
                kitchen_id: kitchens[kitchen],
                id: "score-"+kitchens[kitchen]
            });
            
            this.children.push(unit);
            this.$el.append(unit.$el);
        }
        
        $(window).resize(_.bind(this.render, this));
                
        this.render();

    },

    render: function() {
	var padding = 10;

        this.$el.children().tsort('div.points', {order: 'desc'}, 'h2');
        
        // ensure yellow
        $(".yellow").removeClass('yellow');
        $(this.$el.children().get(0)).addClass('yellow');

        var $units = this.$el.children();
	var totalWidth = this.$el.width();
	var cols = Math.floor( totalWidth / ($units.width() + padding) );
        var height = (Math.ceil(22 / cols) * (80 + padding))+ padding + "px";
        console.log("height: "+ height);
        this.$el.css('height', height);

	$units.each(function(index) {
	    var width = $(this).width();
	    var height = $(this).height();
	    var c = index % cols;
	    var r = Math.floor( index / cols);
	    var left = c * (width + padding);
	    var top = r * (height + padding);
	    left = left % (totalWidth - width);

	    $(this).css({
		left: left,
		top: top
	    });
	});

    }
});

var ScoreboardUnit = Backbone.View.extend({
    tagName: "div",
    className: "scoreboard-unit",
    renderCount: 0,

    initialize: function() {
        this.kitchen_id = this.id.split("-")[1];
        
        if (!this.model.has(this.kitchen_id)) {
            this.model.set(this.kitchen_id, 0);
        }

        this.model.on('change:'+this.kitchen_id, _.bind(this.render, this));
        
        this.$el.html("<h2>"+this.kitchen_id +"</h2><div class=\"points\">0</div><div class=\"odometer\">0</span>");
        this.$el.addClass('animated');

        $(window).resize(_.bind(this.ensureRound, this));
        
        // Ensure that the CSS matches up
        $(_.bind(function() {
            this.render();
        }, this ));        
    },
    render: function() {
        
        if (this.renderCount > 1) {
            this.$el.removeClass('animated').effect('highlight', 'fast').addClass('animated');
        }

        this.$('.points').text(this.model.get(this.kitchen_id));
        this.$('.odometer').text(Math.round(this.model.get(this.kitchen_id)));
        
        this.renderCount++;
        this.ensureRound();
        
    },
    
    ensureRound: function() {
        this.$el.css('height', this.$el.width() + "px");
    }
});


window.odometerOptions = {
    duration: 800,
    theme: 'car',
    animation: 'count' 
};