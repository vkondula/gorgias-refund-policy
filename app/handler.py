from . import config
from .model import db, Transcript
from .logical import decide

from flask import render_template, request, Flask
from flask_bootstrap import Bootstrap

app = Flask(__name__, template_folder='templates')
app.config['SQLALCHEMY_DATABASE_URI'] = config.database_path
db.app = app
db.init_app(app)
Bootstrap(app)


@app.route('/')
@app.route('/index.html')
def index():
    parameters = {
        'serialnumber': request.args.get('serialnumber', '').upper(),
        'repair': request.args.get('repair', ''),
        'caused': request.args.get('additional-0', '') == 'caused',
        'selling': request.args.get('additional-1', '') == 'selling',
    }
    if not parameters['serialnumber']:
        render_template('index.html', parameters=parameters)
    transcript = Transcript.query.filter_by(sn=parameters['serialnumber']).one_or_none()
    pl_parameters = {
        'id': parameters['serialnumber'],
        'exists': True,
        'caused': parameters['caused'],
        'selling': parameters['selling'],
        'price': transcript.price,
        'bought_days_ago': transcript.date,
        'repair_price': parameters['repair'] or False,
        'lifelong_warranty': transcript.lifelong_warranty,
    } if transcript else {'id': parameters['serialnumber']}
    decision, reasoning, logs = decide(pl_parameters, ['reject', 'refund', 'repair', 'new'])
    return render_template(
        'index.html',
        parameters=parameters,
        decision=decision,
        reasoning=reasoning,
        logs=logs,
    )
