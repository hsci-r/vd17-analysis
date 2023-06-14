import getpass

import keyring
import sqlalchemy
import yaml
from hereutil import here
from sqlalchemy import text
from sqlalchemy.exc import SQLAlchemyError
from sqlalchemy.future import Connection, Engine


def get_connection() -> (Engine, Connection):
    eng = None
    con = None
    password = None
    while con is None:
        with here("db_params.yaml").open('r') as f:
            db_params = yaml.safe_load(f)
        if here("db_secret.yaml", warn=False).exists():
            password = yaml.safe_load(here("db_secret.yaml").open('r'))['db_pass']
        else:
            try:
                password = keyring.get_password(db_params['db_name'], "DB_PASS")
            except keyring.errors.NoKeyringError:
                pass
        if password is None:
            password = ""
        try:
            eng = sqlalchemy.create_engine(
                "mariadb+pymysql://" + db_params['db_user'] + ":" + password + "@" + db_params['db_host'] + "/" +
                db_params['db_name'] + "?charset=utf8mb4&autocommit&local_infile",
                isolation_level="AUTOCOMMIT"
            )
            con = eng.connect()
        except SQLAlchemyError as err:
            eng = None
            con = None
            password = getpass.getpass(f"Database password (connection attempt failed with {err}): ")
            if password == "":
                raise ValueError("Empty password given")
            try:
                if keyring.get_password(db_params['db_name'], "DB_PASS") is not None:
                    keyring.delete_password(db_params['db_name'], "DB_PASS")
                keyring.set_password(db_params['db_name'], "DB_PASS", password)
            except keyring.errors.NoKeyringError:
                pass
    return eng, con


def set_session_storage_engine(con: Connection, engine: str):
    con.execute(text("SET SESSION storage_engine="+engine))


__all__ = ["get_connection", "set_session_storage_engine"]
