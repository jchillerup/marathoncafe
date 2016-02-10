cd server
npm install sqlite3 socket.io connect express
mv db.sqlite db.sqlite.bak
cp db.template.sqlite db.sqlite
cd ..
