package jp.co.ctc_g.eim.app.document.presentation.filter;

import java.io.IOException;

import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import eim.bo.EIMUser;
import eim.net.EIMSession;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.common.context.TransactionContext;
import jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum;
import jp.co.ctc_g.eim.framework2.common.enumeration.SessionAttributeNameEnum;

/**
 * スレッドコンテキストにセッションを設定するフィルターです。
 */
public class SetSessionToThreadContextFilter implements Filter {

	/** ログ */
	Log log = LogFactory.getLog(this.getClass().getName());

	/**
	 * init()の実装<br>
	 * コンテキストファイルから情報を取得します。<br>
	 *
	 * @throws ServletException
	 *
	 * @see jakarta.servlet.Filter#init(jakarta.servlet.FilterConfig)
	 */
	public void init(FilterConfig fc) throws ServletException {

	}

	/**
	 * doFilter()の実装
	 * <ul>
	 * <li>スレッドコンテキストにセッションを設定する</li>
	 * </ul>
	 *
	 * @param request HTTPリクエスト
	 * @param response HTTPレスポンス
	 * @param chain Filterチェイン
	 *
	 * @throws IOException
	 * @throws ServletException
	 *
	 * @see jakarta.servlet.Filter#doFilter(jakarta.servlet.ServletRequest, jakarta.servlet.ServletResponse, jakarta.servlet.FilterChain)
	 */
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException {

		HttpServletRequest httpRequest = (HttpServletRequest)request;

		try {
			HttpSession session = httpRequest.getSession(false);
			if (session != null) {
				UserDomain userDomain = (UserDomain)session.getAttribute(SessionAttributeNameEnum.USERDOMAIN.getSymbol());
				String langId = (String)session.getAttribute(SessionAttributeNameEnum.LANG.getSymbol());

				// コネクションを取得するためのセッション情報をセット
				TransactionContext tx = EIMThreadContext.getTransactionContext();
				if (tx == null) {
					tx = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
					EIMThreadContext.putTransactionContext(tx);
				}
				tx.setUser(userDomain);
				tx.setLangId(langId);
				tx.setDBConnection(null);	// コミット対象のコネクションと別になるためDBコネクションはnullにし、トランザクション内で取得させる。。

				// セッションをスレッドコンテキストに設定する
				// documentUpdateNoticeAdviceにて、スレッドコンテキストからセッションを取得している
				EIMSession sess = new EIMSession();

				EIMThreadContext.putEIMSession(sess);
				sess.setDeclarative();
				sess.setUser(convertUserDomainToEIMUser(userDomain));
				sess.setLang(langId);
				sess.setConnection(null);	// コミット対象のコネクションと別になるためDBコネクションはnullにし、トランザクション内で取得させる。
			}

			chain.doFilter(request, response);

		} catch (Exception exception) {
			exception.printStackTrace();

			throw new ServletException(exception);

		} finally {

			try {
				EIMThreadContext.removeEIMSession();
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}

	/**
	 * UserDomainをEIMUserに変換します。
	 * @param userDomain UserDomain
	 * @return EIMUser
	 */
	protected EIMUser convertUserDomainToEIMUser(UserDomain userDomain) {
		return new EIMUser(
				userDomain.getId(),
				userDomain.getCode(),
				userDomain.getName(),
				userDomain.getKana(),
				null, // pass
				userDomain.getMail(),
				userDomain.getAdmin(),
				0, // ログインできているので有効
				userDomain.getDefinitionName()
		);
	}

	/**
	 * destroy()の実装
	 * @see jakarta.servlet.Filter#destroy()
	 */
	public void destroy() {

	}
}
