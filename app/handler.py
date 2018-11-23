from flask import render_template, Flask
from flask_bootstrap import Bootstrap

app = Flask(__name__, template_folder='templates')
Bootstrap(app)


@app.route('/')
@app.route('/index.html')
def index(data=None):
    data = data or {}
    return render_template('index.html', data=data)
