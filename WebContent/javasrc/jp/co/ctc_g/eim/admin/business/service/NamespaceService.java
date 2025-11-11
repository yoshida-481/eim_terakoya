package jp.co.ctc_g.eim.admin.business.service;

import java.util.List;

import jp.co.ctc_g.eim.admin.business.domain.NamespaceDomain;

/**
 * ネームスペースに関する機能を提供するサービスです。
 * @see jp.co.ctc_g.eim.admin.business.dao.NamespaceDao
 * @since Ver6.0
 */

public interface NamespaceService {

	/**
     * 本メソッドは{@link jp.co.ctc_g.eim.admin.business.dao.NamespaceDao#getList() DAO}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.admin.business.dao.NamespaceDao#getList()
	 * @see jp.co.ctc_g.eim.admin.business.service.impl.NamespaceServiceImpl#getList()
	 * @since Ver6.0
	 */
	List<NamespaceDomain> getList() throws Exception;

	/**
     * 本メソッドは{@link jp.co.ctc_g.eim.admin.business.dao.NamespaceDao#getByNamespaceName(String) DAO}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.admin.business.dao.NamespaceDao#getById(String)
	 * @see jp.co.ctc_g.eim.admin.business.service.impl.NamespaceServiceImpl#getById(String)
	 * @since Ver6.0
	 */
	public NamespaceDomain getByNamespaceName(String namespaceName) throws Exception;

}
