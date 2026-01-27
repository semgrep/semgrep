#
# Copyright (c) 2025 Semgrep Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
# LICENSE for more details.
#
import jwt
from mcp.server.auth.provider import AccessToken
from mcp.server.auth.provider import TokenVerifier
from mcp.shared.auth_utils import resource_url_from_server_url

from semgrep.mcp.utilities.utils import get_authorization_server_introspection_endpoint
from semgrep.mcp.utilities.utils import get_authorization_server_jwks_uri
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


class IntrospectionTokenVerifier(TokenVerifier):
    """Token verifier that uses OAuth 2.0 Token Introspection (RFC 7662).

    Copied from the https://github.com/modelcontextprotocol/python-sdk/blob/779271ae384c09884045cbe01302663e72537da6/examples/servers/simple-auth/mcp_simple_auth/token_verifier.py#L12
    """

    def __init__(
        self,
        introspection_endpoint: str,
        jwks_uri: str,
        server_url: str,
        validate_resource: bool = False,
    ):
        self.introspection_endpoint = introspection_endpoint
        self.server_url = server_url
        self.validate_resource = validate_resource
        self.resource_url = resource_url_from_server_url(server_url)
        self._jwks_client = jwt.PyJWKClient(jwks_uri)

    async def verify_token(self, token: str) -> AccessToken | None:
        """Verify token via introspection endpoint."""
        # Validate URL to prevent SSRF attacks
        if not self.introspection_endpoint.startswith(
            ("https://", "http://localhost", "http://127.0.0.1")
        ):
            logger.warning(
                f"Rejecting introspection endpoint with unsafe scheme: {self.introspection_endpoint}"
            )
            return None

        try:
            key = self._jwks_client.get_signing_key_from_jwt(token)
            data = jwt.decode(
                token,
                key,
                algorithms=[key.algorithm_name],
                options={"verify_aud": False},
            )
            return AccessToken(
                token=token,
                client_id=data.get("client_id", "unknown"),
                scopes=data.get("scope", "").split() if data.get("scope") else [],
                expires_at=data.get("exp"),
                resource=data.get("aud"),  # Include resource in token
            )
        except jwt.InvalidTokenError as e:
            logger.warning(f"Invalid token: {e}")
            return None


def make_token_verifier(server_url: str, semgrep_api_url: str) -> TokenVerifier:
    introspection_endpoint = get_authorization_server_introspection_endpoint(
        semgrep_api_url
    )
    jwks_uri = get_authorization_server_jwks_uri(semgrep_api_url)
    return IntrospectionTokenVerifier(
        introspection_endpoint=introspection_endpoint,
        jwks_uri=jwks_uri,
        server_url=server_url,
        validate_resource=True,
    )
