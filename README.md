# Gorgias Refund Policy

## Usage

### Website
Please use http://gorgias.f.seismic.cz/ .
Complete project is running there.

### Installation for Ubuntu
Install system packages:
```
sudo apt-get install python3.7 python3-pip sqlite3
```

Install python packages:
```
pip3 install -r requirements.txt -e .
```

Run gunicorn:
```
gunicorn -c development.py.ini wsgi:app
```

## Database content

Serial number | Price | Date | Lifelong Warranty
--- | --- | --- | ---
K123 | 134.9 | 2018-11-23 | no
H123 | 171.9 | 1995-11-11 | no
M123 | 108.9 | 1995-11-11 | yes
P123 | 426.9 | 2018-11-11 | no

## Package content
```
app/                     # python flask server
app/logical/             # communication with prolog
app/handler.py           # endpoints
libs/kondula-gorgias1.pl # gorgias program
libs/gorgias-src-0.6d/   # gorgias library
```
