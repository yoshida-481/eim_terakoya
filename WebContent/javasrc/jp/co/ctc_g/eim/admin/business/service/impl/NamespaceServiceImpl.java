package jp.co.ctc_g.eim.admin.business.service.impl;

import java.util.List;

import jp.co.ctc_g.eim.admin.business.dao.NamespaceDao;
import jp.co.ctc_g.eim.admin.business.domain.NamespaceDomain;
import jp.co.ctc_g.eim.admin.business.service.NamespaceService;

/**
 * ネームスペースService実装クラス
 * @see jp.co.ctc_g.eim.admin.business.service.NamespaceService
 * @since Ver6.0
 */
public class NamespaceServiceImpl implements NamespaceService {

	/** ネームスペースDao */
	private NamespaceDao namespaceDao;

	/**
	 * 本メソッドは{@link jp.co.ctc_g.eim.admin.business.dao.NamespaceDao#getList() DAO}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.admin.business.dao.NamespaceDao#getList()
	 * @see jp.co.ctc_g.eim.admin.business.service.impl.NamespaceService#getList()
	 * @since Ver6.0
	 */
	public List<NamespaceDomain> getList() throws Exception {
		return namespaceDao.getList();
	}

	/**
	 * 本メソッドは{@link jp.co.ctc_g.eim.admin.business.dao.NamespaceDao#getByNamespaceName(String) DAO}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.admin.business.dao.NamespaceDao#getByNamespaceName(String)
	 * @see jp.co.ctc_g.eim.admin.business.service.impl.NamespaceService#getByNamespaceName(String)
	 * @since Ver6.0
	 */
	public NamespaceDomain getByNamespaceName(String namespaceName) throws Exception {
		return namespaceDao.getByNamespaceName(namespaceName);
	}

	/**
	 * ネームスペースDaoを取得します。
	 * @return ネームスペースDao
	 * @since Ver6.0
	 */
	public void setNamespaceDao(NamespaceDao NamespaceDao) {
	    this.namespaceDao = NamespaceDao;
	}

	/**
	 * ネームスペースDaoを設定します。
	 * @param NamespaceDao ネームスペースDao
	 * @since Ver6.0
	 */
	public NamespaceDao getNamespaceDao() {
	    return namespaceDao;
	}

}
