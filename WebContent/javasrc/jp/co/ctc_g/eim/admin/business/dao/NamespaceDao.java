package jp.co.ctc_g.eim.admin.business.dao;

import java.util.List;
import jp.co.ctc_g.eim.admin.business.domain.NamespaceDomain;

/**
*
* ネームスペースDAO
*
*/
public interface NamespaceDao {

	/**
	 * 全Namespace定義を取得する
	 *
	 * @return ネームスペース一覧
	 * @exception Exception
	 */
	public List<NamespaceDomain> getList() throws Exception;

	/**
	 * 指定のNamespace名に該当するNamespaceDomainを返却する<br>
	 * 該当するNamespaceDomainが見つからない場合、nullを返却する
	 *
	 * @param namespaceName ネームスペース名
	 * @return ネームスペース
	 * @exception Exception
	 */
	public NamespaceDomain getByNamespaceName(String namespaceName) throws Exception;

}
