package jp.co.ctc_g.eim.admin.business.service.impl;

import java.util.List;

import jp.co.ctc_g.eim.admin.business.dao.CacheMonitorViewDao;
import jp.co.ctc_g.eim.admin.business.domain.CacheNodeDomain;
import jp.co.ctc_g.eim.admin.business.domain.criteria.CacheEntrySearchCriteria;
import jp.co.ctc_g.eim.admin.business.service.CacheMonitorViewService;
import jp.co.ctc_g.eim.framework2.business.domain.cache.CacheEntrySearchDomain;
import jp.co.ctc_g.eim.framework2.business.domain.cache.CacheSpaceDomain;

public class CacheMonitorViewServiceImpl implements CacheMonitorViewService {

	/**  */
	CacheMonitorViewDao cacheMonitorViewDao;

	/**
	 * (非 Javadoc)
	 * @see jp.co.ctc_g.eim.admin.business.service.CacheMonitorViewService#getNodes()
	 */
	public List<CacheNodeDomain> getNodes() throws Exception {
		return cacheMonitorViewDao.getNodes();
	}

	/**
	 * (非 Javadoc)
	 * @see jp.co.ctc_g.eim.admin.business.service.CacheMonitorViewService#getCaches(long nodeId)
	 */
	public List<CacheSpaceDomain> getCaches(long nodeId) throws Exception {
		return cacheMonitorViewDao.getCaches(nodeId);
	}

	/**
	 * (非 Javadoc)
	 * @see jp.co.ctc_g.eim.admin.business.service.CacheMonitorViewService#searchEntries(CacheEntrySearchCriteria cacheEntrySearchCriteria)
	 */
	public CacheEntrySearchDomain searchEntries(CacheEntrySearchCriteria cacheEntrySearchCriteria) throws Exception {
		return cacheMonitorViewDao.searchEntries(cacheEntrySearchCriteria);
	}

	/**
	 * (非 Javadoc)
	 * @see jp.co.ctc_g.eim.admin.business.service.CacheMonitorViewService#reload(String cacheSpaceKey, List<String> pkList)
	 */
	public void reload(String cacheSpaceKey, List<String> pkList) throws Exception {
		cacheMonitorViewDao.reload(cacheSpaceKey, pkList);
	}

	/**
	 * キャッシュモニタービューDaoを取得します。
	 * @return キャッシュモニタービューDao
	 */
	public CacheMonitorViewDao getCacheMonitorViewDao() {
		return cacheMonitorViewDao;
	}

	/**
	 * キャッシュモニタービューDaoを設定します。
	 * @param cacheMonitorViewDao キャッシュモニタービューDao
	 */
	public void setCacheMonitorViewDao(CacheMonitorViewDao cacheMonitorViewDao) {
		this.cacheMonitorViewDao = cacheMonitorViewDao;
	}

}
