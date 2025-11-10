package jp.co.ctc_g.eim.app.document.business.service.impl;

import jp.co.ctc_g.eim.app.form.business.domain.FormDomain;

/**
 * HTML用のドキュメント帳票サービス
 */
public class HTMLDocumentFormServiceImpl extends DocumentFormServiceImpl {

	/**
	 * 更新します.
	 * 楽観排他チェックをAOPするために必要
	 */
	public FormDomain update(FormDomain form) throws Exception {
		return super.update(form);
	}
}
