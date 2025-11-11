package jp.co.ctc_g.eim.app.document.business.service;

import java.util.List;

import jp.co.ctc_g.eim.app.form.business.domain.FormDomain;
import jp.co.ctc_g.eim.app.form.business.service.FormService;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;

/**
 * 帳票に関する操作を行うビジネスサービスです。
 * @since Ver 6.6
 */
public interface DocumentFormService extends FormService {

	/**
	 * 指定されたIDの帳票に対応するすべてのリビジョンのオブジェクトを取得します。<br>
	 * 取得したオブジェクトにはIDのみ設定します。
	 * 
	 * @param id 帳票のID<br>
	 * <p style="padding-left:4em">
	 * @return 指定されたIDの帳票の全てのリビジョン<br>
	 * <p style="padding-left:4em">
	 * 該当する帳票がない場合はNULLを返します。
	 * <br><br>
	 * @since Ver 6.6
	 * <br>
	 */
	public List<ObjectDomain> getAllRevisionById(long id) throws Exception;

	/**
	 * 指定されたドキュメントを物理削除します。<br>
	 * 
	 * <br>
	 * @param documentList 削除対象のドキュメントリスト
	 * <p style="padding-left:4em">
	 * form#idで削除対象のドキュメントを特定します。
	 * <br>
	 * @throws Exception 以下の例外を通知します。
	 * <p style="padding-left:4em">
	 * <table width="100%" border="1">
	 * <tr bgcolor="#EEEEFF">
	 *    <th width="100">エラーコード</th>
	 *    <th>原因、処置</th>
	 *  </tr>
 	 *  <tr>
	 *    <td>EIM.ERROR.LOGIC.FORMLIST.VALUE.ILLEGAL</td>
 	 *    <td>引数formListとしてnullが入力されました。</td>
 	 *  </tr>
	 * </table>
	 * @since Ver 1.0
	 * <br>
	 */
	public void deleteDocument(List<FormDomain> documentList) throws Exception;

	/**
	 * 指定された帳票のリビジョンのみ物理削除します。<br>
	 * 
	 * <br>
	 * @param formList 削除対象の帳票リスト
	 * <p style="padding-left:4em">
	 * form#idで削除対象の帳票を特定します。
	 * <br>
	 * @throws Exception 以下の例外を通知します。
	 * <p style="padding-left:4em">
	 * <table width="100%" border="1">
	 * <tr bgcolor="#EEEEFF">
	 *    <th width="100">エラーコード</th>
	 *    <th>原因、処置</th>
	 *  </tr>
 	 *  <tr>
	 *    <td>EIM.ERROR.LOGIC.FORMLIST.VALUE.ILLEGAL</td>
 	 *    <td>引数formListとしてnullが入力されました。</td>
 	 *  </tr>
	 * </table>
	 * @since Ver 6.6
	 * <br>
	 */
	public void deleteTargetRevision(List<FormDomain> formList) throws Exception;

	/**
	 * 指定されたIDの帳票を権限を参照せずに取得します。<br>
	 *
	 * @param id 帳票のID<br>
	 * <p style="padding-left:4em">
	 * @return 指定されたIDの帳票<br>
	 * <p style="padding-left:4em">
	 * 該当する帳票がない場合はNULLを返します。
	 * <br><br>
	 * @since Ver 1.0
	 * <br>
	 */
	public FormDomain getByIdAdmin(long id) throws Exception;
	
}
