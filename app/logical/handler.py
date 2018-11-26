from pyswip import Prolog

from .config import DYNAMIC_RECORDS
from .. import config


def _get_reasons(session: Prolog, results: list) -> (str, list, list):
    variable = 'Delta'
    queries = []
    for result in results:
        query = f"prove([{result}(item)], {variable})."
        queries.append(query)
        response = session.query(query)
        for solution in response:
            return result, [a.value for a in solution[variable]], queries
    return '', [], queries


def _clean_session(session: Prolog) -> None:
    for param_count, records in DYNAMIC_RECORDS.items():
        for record in records:
            parameters = ', '.join(['_' for _ in range(param_count)])
            session.retractall(f"{record}({parameters})")


def decide(parameters: dict, results: list) -> (str, list, list):
    # Create new prolog session
    session = Prolog()
    # Load file and Grogias
    session.consult(config.prolog_path)
    _clean_session(session)
    # Assert rules
    logs = []
    parameters.pop('id', None)
    for key, value in parameters.items():
        if value is True:
            # e.g. {key: 'sold', value: True, id: 1} -> assertz(sold('1'))
            query = f"assertz({key}(item))."
            # session.query(query)
            session.assertz(f"{key}(item)")
            logs.append(query)
        elif str(value).replace('.', '').isdigit():
            # e.g. {key: 'price', value: 100, id: 1} -> assertz(price('1', 100))
            query = f"assertz({key}(item, {value}))."
            # session.query(query)
            session.assertz(f"{key}(item, {value})")
            logs.append(query)
        elif value is not False:
            # e.g. {key: 'price', value: 'sold', id: 1} -> assertz(price('1', 'sold'))
            query = f"assertz({key}(item, '{value}'))."
            # session.query(query)
            session.assertz(f"{key}(item, '{value}')")
            logs.append(query)
    # decide
    decision, reasons, reasons_logs = _get_reasons(session, results)
    logs.extend(reasons_logs)
    return decision, reasons, logs
