from pyswip import Prolog

from .. import config


def _get_reasons(session: Prolog, identity: str, results: list) -> (str, list, str):
    variable = 'Delta'
    for result in results:
        query = f'prove([{result}("{identity}")], {variable}).'
        response = session.query(query)
        for solution in response:
            return result, [a.value for a in solution[variable]], query
    return '', []


def decide(parameters: dict, results: list) -> (str, list, list):
    # Create new prolog session
    session = Prolog()
    # Load file and Grogias
    session.consult(config.prolog_path)
    # Assert rules
    logs = []
    identity = parameters.pop('id')
    for key, value in parameters.items():
        if value is True:
            # e.g. {key: 'sold', value: True, id: 1} -> assertz(sold(1))
            query = f'assertz({key}("{identity}"))'
            session.query(query)
            logs.append(query)
        else:
            # e.g. {key: 'price', value: 100, id: 1} -> assertz(price(1, 100))
            query = f'assertz({key}("{identity}", "{value}"))'
            session.query(query)
            logs.append(query)
    # decide
    decision, reasons, log = _get_reasons(session, identity, results)
    logs.append(log)

    return decision, reasons, logs
