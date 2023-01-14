import javax.ws.rs.client.WebTarget
import javax.servlet.*
import org.springframework.stereotype.Service
import javax.annotation.PostConstruct
import org.springframework.beans.factory.annotation.Qualifier

//ERROR: match
@Service
class SsoFilter(
    @Qualifier("ssoService.ssoClient") ssoTarget: WebTarget)  : Filter {

    @PostConstruct
    fun init() {
        this.siteSsoTarget = siteSsoTarget
    }
    override fun doFilter(request: ServletRequest, response: ServletResponse, chain: FilterChain) {
        val context = ServletRequestContext(request as HttpServletRequest, response as HttpServletResponse)
    }

}

