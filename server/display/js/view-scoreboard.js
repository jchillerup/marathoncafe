var Scoreboard = Backbone.View.extend({
    tagName: "section",
    id: "scoreboard",
    children: [],

    initialize: function() {
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
        
        $(_.bind(function() {
            setTimeout(this.enableEffects, 2000);
        }, this));
    },
    enableEffects: function() {
        console.log('enabling effects');
        $(".scoreboard-unit").css({"transition": "all 1s", "-webkit-transition": "all 1s"});
        window.odometerOptions.duration = 800;
    },
    render: function() {
        console.log("scoreboard render");
	var padding = 20;
        var offsetTop = 60;

        this.$el.children().tsort('div.points', {order: 'desc'});

        var $units = this.$el.children();
	var totalWidth = $(document.body).width();
	var cols = Math.floor( totalWidth / ($units.width() + padding) );
	$units.each(function(index) {
		var width = $(this).width();
		var height = $(this).height();
		var c = index % cols;
		var r = Math.floor( index / cols);
		var left = c * (width + padding);
		var top = r * (height + padding) + offsetTop;
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
    firstTime: true,

    initialize: function() {
        this.kitchen_id = this.id.split("-")[1];
        
        if (!this.model.has(this.kitchen_id)) {
            this.model.set(this.kitchen_id, 0);
        }

        this.model.on('change:'+this.kitchen_id, _.bind(this.render, this));
        
        this.$el.html("<h2>"+this.kitchen_id +"</h2><div class=\"points\">0</div><div class=\"odometer\">0</span>");
        
        this.render();
        
        $(window).resize(_.bind(this.ensureRound, this));
    },
    render: function() {
        
        if (!this.firstTime) {
            this.$el.effect('highlight');
        }

        this.$('.points').text(this.model.get(this.kitchen_id));
        this.$('.odometer').text(this.model.get(this.kitchen_id));
       
        this.firstTime = false;
        this.ensureRound();
        
    },
    
    ensureRound: function() {
        this.$el.css('height', this.$el.width() + "px");
        // if (this.$el.height() > this.$el.width()) {
        //     this.$el.css('height', this.$el.width() + "px");
        // } else {
        //     this.$el.css('height', '');
        // }
    }
});


window.odometerOptions = {
    duration: 800,
    theme: 'car',
    animation: 'count' 
};