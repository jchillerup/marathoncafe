var PlotsView = Backbone.View.extend({
    id: "plots",
    num_plots: 14,
    cur_plot: 0,
    $cur_img: null,

    events: {
        "click img": "advance"
    },
    
    initialize: function() {
        console.log('plots view initialized');
        
        this.insertPic();
        this.insertPic();
        this.advance();
        setInterval(_.bind(this.advance, this), 15000);
    },
    
    insertPic: function() {
        var path = "plots/plot"+(this.cur_plot + 1)+".png?cachebust="+(new Date()).getTime();
        var $img = $("<img>").attr('src', path).css('left', '480px');

        this.$el.append($img); 
        this.$cur_img = $img;
    },

    advance: function(event) {
        this.cur_plot += 1;
        this.cur_plot %= this.num_plots;
        
        this.insertPic();

        this.$("img:first").animate({"left": "-480px"}, 600, function() {$(this).remove();});
        $(this.$("img").get(1)).animate({"left": "0px"}, 600);
        
    }
});
