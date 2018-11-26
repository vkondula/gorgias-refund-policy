from datetime import date

from flask import render_template, request, Flask
from flask_bootstrap import Bootstrap

from . import config
from .model import db, Transcript
from .logical import decide


app = Flask(__name__, template_folder="templates")
app.config["SQLALCHEMY_DATABASE_URI"] = config.database_path
db.app = app
db.init_app(app)
Bootstrap(app)


@app.route("/")
@app.route("/index.html")
def index():
    parameters = {
        "serialnumber": request.args.get("serialnumber", "").upper(),
        "repair": request.args.get("repair", ""),
        "caused": request.args.get("additional-0", "") == "caused",
        "selling": request.args.get("additional-1", "") == "selling",
    }
    if not parameters["serialnumber"]:
        return render_template("index.html", parameters=parameters)
    transcript = Transcript.query.filter_by(sn=parameters["serialnumber"]).one_or_none()
    pl_parameters = {"id": parameters["serialnumber"], "considered": True}
    if transcript:
        pl_parameters = {
            "id": parameters["serialnumber"],
            "considered": True,
            "exists": True,
            "caused": parameters["caused"],
            "selling": parameters["selling"],
            "price": transcript.price,
            "bought_days_ago": abs((transcript.date - date.today()).days),
            "repair_price": parameters["repair"] or False,
            "lifelong_warranty": transcript.lifelong_warranty,
        }
        parameters["name"] = transcript.name
        parameters["price"] = transcript.price
        parameters["date"] = transcript.date
    decision, reasoning, logs = decide(
        pl_parameters, ["repair", "exchange", "refund", "reject"]
    )
    return render_template(
        "index.html",
        parameters=parameters,
        decision=decision.upper(),
        reasoning=reasoning,
        logs=list(enumerate(logs, start=1)),
    )
