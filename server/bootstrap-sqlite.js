var sqlite3 = require('sqlite3').verbose();

var db = new sqlite3.Database('maratoncafe.sqlite');

db.serialize(function() {
    db.run('CREATE TABLE streger ()');
    
});


db.close();