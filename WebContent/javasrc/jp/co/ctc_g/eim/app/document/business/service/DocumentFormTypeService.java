package jp.co.ctc_g.eim.app.document.business.service;

import jp.co.ctc_g.eim.app.form.business.domain.FormTypeDomain;
import jp.co.ctc_g.eim.app.form.business.service.FormTypeService;

/**
 * 帳票タイプに関する操作を行うビジネスサービスです。
 * @since Ver 1.0
 */
public interface DocumentFormTypeService extends FormTypeService {


	/**
	 * 指定されたIDの文書タイプを取得します。<br>
	 * <br>
	 * @param id 文書タイプID
	 * @param parentId 親オブジェクトID(ワークスペース、または、フォルダ)
	 * @return 文書タイプ
	 */
	public FormTypeDomain getByIdAndParent(long id, long parentId) throws Exception;

}
