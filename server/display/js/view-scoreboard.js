var Scoreboard = Backbone.View.extend({
    tagName: "section",
    id: "scoreboard",
    children: [],

    initialize: function(arg1, arg2) {
        // Create all the kitchen widgets
        var kitchens = [
            "GL1", "GL2", "GL3", "GL4", "GL5", "GL6", "GL7", "GL8",
                   "ML2", "ML3", "ML4", "ML5", "ML6", "ML7", "ML8",
                   "NY2", "NY3", "NY4", "NY5", "NY6", "NY7", "NY8"
        ];

        for (var kitchen in kitchens) {
            var unit = new ScoreboardUnit({
                model: this.model,
                kitchen_id: kitchens[kitchen],
                id: "score-"+kitchens[kitchen],
                scoreboard: this
            });

            this.children.push(unit);
            this.$el.append(unit.$el);
        }

        // Ensure the scoreboard is redrawn when the window is resized.
        // This is needed to re-sort the scoreboard units, if needed.
        $(window).resize(_.bind(this.render, this));
        
        // And when scores change, we need to ensure that we still have
        // the right sorting.
        this.model.get('scores').on('change', _.bind(function() {
            setTimeout(_.bind(function() {
                this.render();
            }, this), 100);
        }, this));

        this.render();
    },

    render: _.debounce(function() {
        // console.log('scoreboard render');
        var padding = 10;

        var $units = this.$el.children();
        var totalWidth = this.$el.width();
        var cols = Math.floor( totalWidth / ($units.width() + padding) );
        // var height = (Math.ceil(28 / cols) * (80 + padding))+ padding + "px";
        // console.log("height: "+ height);
        // this.$el.css('height', "10%");

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
        

        // Sort the scoreboard elements by kitchen.
        tinysort('#scoreboard div.scoreboard-unit', {selector: '.points', order: 'desc'});
        
        // ensure yellow
        $(".yellow").removeClass('yellow');
        $(this.$el.children().get(0)).addClass('yellow');
    },
                       5) // debounce the function so we can have all scoreboardunits call 
                          // render without spamming the scoreboard.
}

);

var ScoreboardUnit = Backbone.View.extend({
    tagName: "div",
    className: "scoreboard-unit",
    renderCount: 0,
    scoreboardView: null,

    initialize: function(options) {
        this.kitchen_id = this.id.split("-")[1];

        if (!this.model.get('scores').has(this.kitchen_id)) {
            this.model.get('scores').set(this.kitchen_id, 0);
        }

        this.model.get('scores').on('change', _.bind(this.render, this));

        this.$el.html("<div class=\"kitchen\"><div class=\"tc\">"+this.kitchen_id +"</div></div>"
                      + "<div class=\"bar bar_points\">"
                      + "<div class=\"percentage percentage_points\"></div>"
                      + "<div class=\"points\">0</div>"
                      + "</div>"
                      + "<div class=\"bar bar_momentum\"> "
                      + "<div class=\"percentage percentage_momentum\"></div>"
                      + "<div class=\"momentum\">0</div>"
                      + "</div>"
                      + "");
        this.$el.addClass('animated');

        // Ensure that the CSS matches up
        $(_.bind(function() {
            this.render();
        }, this ));
        
        this.scoreboardView = options.scoreboard;
    },

    render: function() {
        // if (this.renderCount > 1) {
        //     this.$el.removeClass('animated').effect('highlight', 'fast').addClass('animated');
        // }

        var points = this.model.get('scores').get(this.kitchen_id);
        var momentum = this.model.get('momentum').get(this.kitchen_id);

	// Determine if we need animation
	if (this.$('.points').text() != ""+Math.round(points)) {
	    //	    this.$el.removeClass('animated').effect('highlight', 'fast').addClass('animated');
	    this.$el.children().effect('highlight', 'slow');
	}
	
        this.$('.points').text(Math.round(points));
        this.$('.momentum').text(Math.round(momentum));

        var maxPoints = _.max(this.model.get('scores').attributes);
        var maxMomentum = _.max(this.model.get('momentum').attributes);

        var percentagePoints = points/maxPoints;
        var percentageMomentum = momentum/maxMomentum;

        this.$('.bar_points .percentage').width((100 * percentagePoints) + "%");
        this.$('.bar_momentum .percentage').width((100 * percentageMomentum) + "%");

        // console.log([
        //     percentagePoints,
        //     percentageMomentum]);

        this.renderCount++;
        
        this.scoreboardView.render();
    }
});


window.odometerOptions = {
    duration: 800,
    theme: 'car',
    animation: 'count'
};
