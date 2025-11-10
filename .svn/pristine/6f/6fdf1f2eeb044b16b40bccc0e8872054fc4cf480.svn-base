package jp.co.ctc_g.eim.app.document.common.security;

import java.io.IOException;
import java.util.Locale;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.security.core.Authentication;

import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.DateUtils;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.common.enumeration.SessionAttributeNameEnum;
import jp.co.ctc_g.eim.framework2.common.security.EIMSpnegoAuthenticationSuccessHandler;

/**
 * ドキュメント管理用SPNEGO認証成功ハンドラ
 *
 * @see org.springframework.security.web.authentication.AuthenticationSuccessHandler
 */
public class EIMDocumentSpnegoAuthenticationSuccessHandler extends EIMSpnegoAuthenticationSuccessHandler {

	/** Logger */
	private static final Log log = LogFactory.getLog(EIMDocumentSpnegoAuthenticationSuccessHandler.class);

	/**
	 * 認証成功時の処理です。
	 * <p>
	 * EIMセッション情報を設定します。
	 * EIMSpnegoAuthenticationSuccessHandlerで設定する情報の他に、ドキュメント管理用の情報を追加で設定します。
	 * </p>
	 * @param request HTTPサーブレットリクエスト
	 * @param response HTTPサーブレットレスポンス
	 * @param authentication 認証オブジェクト
	 * @see org.springframework.security.web.authentication.AuthenticationSuccessHandler#onAuthenticationSuccess(jakarta.servlet.http.HttpServletRequest, jakarta.servlet.http.HttpServletResponse, org.springframework.security.core.Authentication)
	 * @see jp.co.ctc_g.eim.framework2.common.security.EIMSpnegoAuthenticationSuccessHandler
	 */
	@Override
	public void onAuthenticationSuccess(HttpServletRequest request, HttpServletResponse response,
			Authentication authentication) throws IOException, ServletException {

		// スーパークラスを呼び出しFWで必要なセッション情報を設定する
		super.onAuthenticationSuccess(request, response, authentication);

		// セッション情報を取得
		HttpSession session = request.getSession(false);
		UserDomain userDomain = (UserDomain) session.getAttribute(SessionAttributeNameEnum.USERDOMAIN.getSymbol());
		EIMUser user = convert(userDomain);
		String langId = (String) session.getAttribute(SessionAttributeNameEnum.LANG.getSymbol());

		// ドキュメント管理用のセッション情報を設定
		session.setAttribute("USER", user);
		session.setAttribute("locale", new Locale(langId));

		EIMSession sess = null;
		long dbTzOffset = 0;

		try {
			sess = new EIMSession(request);

			// DBサーバ端末のタイムゾーンとGMT標準値の時差を取得
			dbTzOffset = DateUtils.selectDBTzOffset(sess);

		} catch (Exception e) {
			throw new IOException(e);
		} finally {
			if (sess != null) {
				try {
					sess.close();
				} catch (Exception e) {
					log.error("", e);
				}
			}
		}
		session.setAttribute("dbTzOffset", Long.toString(dbTzOffset));
	}

	/**
	 * UserDomainからEIMUserへ変換します。パスワードはnullとします。
	 * @param userDomain
	 * @return
	 */
	private EIMUser convert(UserDomain userDomain) {
		EIMUser user = new EIMUser(userDomain.getId(), userDomain.getCode(), userDomain.getName(), userDomain.getKana(),
				null, userDomain.getMail(), userDomain.getAdmin(), (userDomain.isDisable() ? 1 : 0),
				userDomain.getDefinitionName(), userDomain.getLang());
		return user;
	}
}
