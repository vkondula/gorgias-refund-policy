from flask_sqlalchemy import SQLAlchemy

db = SQLAlchemy()


class Transcript(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.Text, unique=True, nullable=False)
    sn = db.Column(db.Text, unique=True, index=True, nullable=False)
    price = db.Column(db.DECIMAL(2), nullable=False)
    date = db.Column(db.Date, nullable=False)
    lifelong_warranty = db.Column(db.Boolean, nullable=False, default=False)
