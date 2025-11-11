package jp.co.ctc_g.eim.app.document.business.service;

import java.io.File;

import jp.co.ctc_g.eim.app.form.business.domain.FormDomain;
import jp.co.ctc_g.eim.app.form.business.service.FormService;
import jp.co.ctc_g.eim.framework2.common.exception.EIMApplicationException;

/**
 * スキャン用表紙に関する操作を行うビジネスサービスです。
 * @since Ver6.6
 */
public interface CoverDocumentFormService extends FormService {

	/**
	 * 指定された条件でスキャン用表紙を作成します。<br>
	 * 名称による重複チェックは行いません。<br>
	 *
	 * @param form 作成対象の表紙（帳票）
	 * @return 作成された帳票
	 * @since Ver 6.6
	 * <br>
	 */
	public FormDomain createCover(FormDomain form) throws Exception;

	/**
	 * ファイルなしドキュメントを登録します。
	 * @param form 登録対象
	 * @param parentForm 親
	 * @return 登録したドキュメント
	 * @throws Exception
	 * @throws EIMApplicationException
	 * @since Ver6.6
	 */
	public FormDomain createWithoutFile(FormDomain form, FormDomain parentForm) throws Exception;
	
	/**
	 * 権限チェックを行わずにファイルなしドキュメントを登録します。
	 * @param form 登録対象
	 * @param parentForm 親
	 * @return 登録したドキュメント
	 * @throws Exception
	 * @throws EIMApplicationException
	 * @since Ver6.6
	 */
	public FormDomain createWithoutFileForce(FormDomain form, FormDomain parentForm) throws Exception;

	/**
	 * ドキュメントの原本フォーマットにファイルをチェックインします。
	 * @param documentForm 登録対象のドキュメント
	 * @param parentForm 親
	 * @param targetFile チェックインするファイル
	 * @since Ver6.6
	 */
	public void checkInFile(FormDomain documentForm, FormDomain parentForm, File targetFile) throws Exception;
}
