package jp.co.ctc_g.eim.app.document.business.service;


import java.util.List;

import jp.co.ctc_g.eim.framework2.business.domain.criteria.SecurityCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.SecurityDomain;
import jp.co.ctc_g.eim.framework2.business.service.SecurityService;
/**
* 【ドキュメントAPI】
* ドキュメント管理セキュリティサービスクラス
*@since Ver1.0
*/
public interface DocumentSecurityService extends SecurityService {

	/**
	 * セキュリティIDを指定してセキュリティを取得します。
	 *
	 * @param id セキュリティID
	 * @return セキュリティドメイン
	 * @throws Exception
	 */
	public SecurityDomain getById(long id) throws Exception;

	/**
	 * セキュリティ名を指定してセキュリティを取得します。
	 *
	 * @param name セキュリティ名
	 * @return セキュリティドメイン
	 * @throws Exception
	 */
	public SecurityDomain getByDefinitionName(String name) throws Exception;

	/**
	 * セキュリティクライテリア指定してセキュリティを取得します。
	 *
	 * @param criteria セキュリティクライテリア
	 * @return セキュリティドメインリスト
	 * @throws Exception
	 */
	public List<SecurityDomain> getList(SecurityCriteria criteria) throws Exception;

	/**
	 * セキュリティを作成します。
	 *
	 * @param domain セキュリティドメイン
	 * @return セキュリティドメイン
	 * @throws Exception
	 */
	public SecurityDomain create(SecurityDomain domain) throws Exception;

	/**
	 * セキュリティを更新します。
	 *
	 * @param domain セキュリティドメイン
	 * @throws Exception
	 */
	public void update(SecurityDomain domain) throws Exception;

	/**
	 * セキュリティを削除します。
	 *
	 * @param domain セキュリティドメイン
	 * @throws Exception
	 */
	public void delete(SecurityDomain domain) throws Exception;
}
