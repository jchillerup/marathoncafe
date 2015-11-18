var PlotsView = Backbone.View.extend({
    id: "plots",
    cur_plot: 0,
    $cur_img: null,
    plotfiles: ['plot0.png', 'plot11.png', 'plot12.png', 'plot13.png', 'plot1.png', 'plot2.png', 'plot4.png', 'plot6.png', 'plot7.png', 'plot9.png'],

    events: {
        "click img": "advance"
    },
    
    initialize: function() {
        console.log('plots view initialized');
        
        this.insertPic();
        this.insertPic();
        this.advance();

        setInterval(_.bind(this.advance, this), 18000);
    },
    
    insertPic: function() {
        var path = "plots/"+(this.plotfiles[this.cur_plot])+"?cachebust="+(new Date()).getTime();
        var $img = $("<img>").attr('src', path).css('left', '480px');

        this.$el.append($img);
        this.$cur_img = $img;
    },

    advance: function(event) {
        this.cur_plot += 1;
        this.cur_plot %= this.plotfiles.length;
        
        this.insertPic();

        this.$("img:first").animate({"left": "-480px"}, 600, function() {$(this).remove();});
        $(this.$("img").get(1)).animate({"left": "0px"}, 600);       
    }
});
