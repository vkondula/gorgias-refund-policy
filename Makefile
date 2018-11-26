black:
	black -l 88 --py36 app setup.py wsgi.py

dev:
	gunicorn -c development.py.ini wsgi:app

.PHONY: black
