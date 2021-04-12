# Original source: semgrep/perf/bench/netflix/input/lemur/tests/conftest.py
# type: ignore
import datetime
import os

import pytest
from cryptography import x509
from cryptography.hazmat.backends import default_backend
from cryptography.hazmat.primitives import hashes
from flask import current_app
from flask_principal import Identity
from flask_principal import identity_changed
from lemur import create_app
from lemur.auth.service import create_token
from lemur.common.utils import parse_private_key
from lemur.database import db as _db
from lemur.tests.vectors import INTERMEDIATE_KEY
from lemur.tests.vectors import ROOTCA_CERT_STR
from lemur.tests.vectors import ROOTCA_KEY
from lemur.tests.vectors import SAN_CERT_KEY
from sqlalchemy.sql import text

from .factories import ApiKeyFactory
from .factories import AsyncAuthorityFactory
from .factories import AuthorityFactory
from .factories import CACertificateFactory
from .factories import CertificateFactory
from .factories import CryptoAuthorityFactory
from .factories import DestinationFactory
from .factories import EndpointFactory
from .factories import InvalidCertificateFactory
from .factories import NotificationFactory
from .factories import PendingCertificateFactory
from .factories import RoleFactory
from .factories import RotationPolicyFactory
from .factories import SourceFactory
from .factories import UserFactory


def pytest_runtest_setup(item):
    if "slow" in item.keywords and not item.config.getoption("--runslow"):
        pytest.skip("need --runslow option to run")

    if "incremental" in item.keywords:
        previousfailed = getattr(item.parent, "_previousfailed", None)
        if previousfailed is not None:
            pytest.xfail("previous test failed ({0})".format(previousfailed.name))


def pytest_runtest_makereport(item, call):
    if "incremental" in item.keywords:
        if call.excinfo is not None:
            parent = item.parent
            parent._previousfailed = item


@pytest.fixture(scope="session")
def app(request):
    """
    Creates a new Flask application for a test duration.
    Uses application factory `create_app`.
    """
    _app = create_app(
        config_path=os.path.dirname(os.path.realpath(__file__)) + "/conf.py"
    )
    ctx = _app.app_context()
    ctx.push()

    yield _app

    ctx.pop()


@pytest.fixture(scope="session")
def db(app, request):
    _db.drop_all()
    _db.engine.execute(text("CREATE EXTENSION IF NOT EXISTS pg_trgm"))
    _db.create_all()

    _db.app = app

    UserFactory()
    r = RoleFactory(name="admin")
    u = UserFactory(roles=[r])
    rp = RotationPolicyFactory(name="default")
    ApiKeyFactory(user=u)

    _db.session.commit()
    yield _db
    _db.drop_all()


@pytest.fixture(scope="function")
def session(db, request):
    """
    Creates a new database session with (with working transaction)
    for test duration.
    """
    db.session.begin_nested()
    yield db.session
    db.session.rollback()


@pytest.fixture(scope="function")
def client(app, session, client):
    yield client


@pytest.fixture
def authority(session):
    a = AuthorityFactory()
    session.commit()
    return a


@pytest.fixture
def crypto_authority(session):
    a = CryptoAuthorityFactory()
    session.commit()
    return a


@pytest.fixture
def async_authority(session):
    a = AsyncAuthorityFactory()
    session.commit()
    return a


@pytest.fixture
def destination(session):
    d = DestinationFactory()
    session.commit()
    return d


@pytest.fixture
def source(session):
    s = SourceFactory()
    session.commit()
    return s


@pytest.fixture
def notification(session):
    n = NotificationFactory()
    session.commit()
    return n


@pytest.fixture
def certificate(session):
    u = UserFactory()
    a = AuthorityFactory()
    c = CertificateFactory(user=u, authority=a)
    session.commit()
    return c


@pytest.fixture
def endpoint(session):
    s = SourceFactory()
    e = EndpointFactory(source=s)
    session.commit()
    return e


@pytest.fixture
def role(session):
    r = RoleFactory()
    session.commit()
    return r


@pytest.fixture
def user(session):
    u = UserFactory()
    session.commit()
    user_token = create_token(u)
    token = {"Authorization": "Basic " + user_token}
    return {"user": u, "token": token}


@pytest.fixture
def pending_certificate(session):
    u = UserFactory()
    a = AsyncAuthorityFactory()
    p = PendingCertificateFactory(user=u, authority=a)
    session.commit()
    return p


@pytest.fixture
def pending_certificate_from_full_chain_ca(session):
    u = UserFactory()
    a = AuthorityFactory()
    p = PendingCertificateFactory(user=u, authority=a)
    session.commit()
    return p


@pytest.fixture
def pending_certificate_from_partial_chain_ca(session):
    u = UserFactory()
    c = CACertificateFactory(body=ROOTCA_CERT_STR, private_key=ROOTCA_KEY, chain=None)
    a = AuthorityFactory(authority_certificate=c)
    p = PendingCertificateFactory(user=u, authority=a)
    session.commit()
    return p


@pytest.fixture
def invalid_certificate(session):
    u = UserFactory()
    a = AsyncAuthorityFactory()
    i = InvalidCertificateFactory(user=u, authority=a)
    session.commit()
    return i


@pytest.fixture
def admin_user(session):
    u = UserFactory()
    admin_role = RoleFactory(name="admin")
    u.roles.append(admin_role)
    session.commit()
    user_token = create_token(u)
    token = {"Authorization": "Basic " + user_token}
    return {"user": u, "token": token}


@pytest.fixture
def async_issuer_plugin():
    from lemur.plugins.base import register
    from .plugins.issuer_plugin import TestAsyncIssuerPlugin

    register(TestAsyncIssuerPlugin)
    return TestAsyncIssuerPlugin


@pytest.fixture
def issuer_plugin():
    from lemur.plugins.base import register
    from .plugins.issuer_plugin import TestIssuerPlugin

    register(TestIssuerPlugin)
    return TestIssuerPlugin


@pytest.fixture
def notification_plugin():
    from lemur.plugins.base import register
    from .plugins.notification_plugin import TestNotificationPlugin

    register(TestNotificationPlugin)
    return TestNotificationPlugin


@pytest.fixture
def destination_plugin():
    from lemur.plugins.base import register
    from .plugins.destination_plugin import TestDestinationPlugin

    register(TestDestinationPlugin)
    return TestDestinationPlugin


@pytest.fixture
def source_plugin():
    from lemur.plugins.base import register
    from .plugins.source_plugin import TestSourcePlugin

    register(TestSourcePlugin)
    return TestSourcePlugin


@pytest.fixture(scope="function")
def logged_in_user(session, app):
    with app.test_request_context():
        identity_changed.send(current_app._get_current_object(), identity=Identity(1))
        yield


@pytest.fixture(scope="function")
def logged_in_admin(session, app):
    with app.test_request_context():
        identity_changed.send(current_app._get_current_object(), identity=Identity(2))
        yield


@pytest.fixture
def private_key():
    return parse_private_key(SAN_CERT_KEY)


@pytest.fixture
def issuer_private_key():
    return parse_private_key(INTERMEDIATE_KEY)


@pytest.fixture
def cert_builder(private_key):
    return (
        x509.CertificateBuilder()
        .subject_name(
            x509.Name([x509.NameAttribute(x509.NameOID.COMMON_NAME, "foo.com")])
        )
        .issuer_name(
            x509.Name([x509.NameAttribute(x509.NameOID.COMMON_NAME, "foo.com")])
        )
        .serial_number(1)
        .public_key(private_key.public_key())
        .not_valid_before(datetime.datetime(2017, 12, 22))
        .not_valid_after(datetime.datetime(2040, 1, 1))
    )


@pytest.fixture
def selfsigned_cert(cert_builder, private_key):
    # cert_builder uses the same cert public key as 'private_key'
    return cert_builder.sign(private_key, hashes.SHA256(), default_backend())


@pytest.fixture(scope="function")
def aws_credentials():
    os.environ["AWS_ACCESS_KEY_ID"] = "testing"
    os.environ["AWS_SECRET_ACCESS_KEY"] = "testing"
    os.environ["AWS_SECURITY_TOKEN"] = "testing"
    os.environ["AWS_SESSION_TOKEN"] = "testing"
