package jp.co.ctc_g.eim.admin.presentation.web.controller;

import jp.co.ctc_g.eim.app.form.presentation.controller.RestController;
import jp.co.ctc_g.eim.framework2.common.context.EIMFlexContext;

/**
 * Rest呼び出し（Admin用拡張）ベースコントロールクラス
 * Spring MVC のコントローラクラスです。
 * 
 * @since Ver3.0
 */
public class AdminRestController extends RestController {

	/**
	 * FlexContextをクリアします。
	 * JSPで帳票サービスを使用している箇所があり、FlexContextが残った状態だと
	 * SessionTimeoutCheckAdviceにてセッションタイムアウトと判定されるためシステム管理ではクリアします。
	 */
	protected void clearFlexContext() {
		EIMFlexContext.setThreadLocalHttpRequest(null);
		EIMFlexContext.setThreadLocalHttpResponse(null);
	}
}
