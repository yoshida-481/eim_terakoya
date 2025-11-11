package jp.co.ctc_g.eim.app.document.common.util;

import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.springframework.context.ApplicationContext;

import jp.co.ctc_g.eim.framework2.business.dao.ObjectDao;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.ObjectCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;

/**
 * 【ドキュメントAPI】
 */
public class VersionUtils {

	/**
	 * オブジェクトのバージョンを取得します。
	 *
	 * @param object
	 * @return
	 * @throws Exception
	 */
	public static Map<Long, ObjectDomain> getVersion(ObjectDomain object) throws Exception {
		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		ObjectService objectService = (ObjectService) context.getBean("objectService2");

		// Variables
		Map<Long, ObjectDomain> revObjectMap = new LinkedHashMap<Long, ObjectDomain>();

		if(object.getId() <= 0){
			return revObjectMap;
		}

		try {
			// レビジョングループID取得
			ObjectDao objectDao = (ObjectDao) context.getBean("objectDaoForUtilForGetVersion");
			ObjectDomain objectDomain = objectDao.getById(object.getId());
			if (objectDomain == null) {
				// オブジェクトが取得できません。
				throw new EIMException("EIM.ERROR.OBJECT.NOTFOUND");
			}

			// オブジェクト一覧取得
			ObjectCriteria criteria = new ObjectCriteria();
			long versionId = objectDomain.getRevisionGroupId();
			criteria.setRevisionGroupId(versionId);

			List<ObjectDomain> objectDomainList = objectService.getList(criteria);
			if (objectDomainList == null || objectDomainList.size() == 0) {
				// 指定したバージョンが存在しません。
				throw new EIMException("EIM.ERROR.LOGIC.VERSION.NOTFOUND");
			}

			if(objectDomainList.size() == 0){
				return revObjectMap;
			}

			// 履歴の昇順でソートする
			VersionUtils versionUtils = new VersionUtils();
			Collections.sort(objectDomainList, versionUtils.new ObjectDomainComparator());

			// マップに格納
			for(ObjectDomain obj : objectDomainList){
				revObjectMap.put(Long.valueOf(obj.getRevision()), obj);
			}

			return revObjectMap;
		} catch (jp.co.ctc_g.eim.framework2.common.exception.EIMException e) {
			// Daoの例外をUtilの例外に変換
			EIMException ee = new EIMException(e.getMessageKey());
			ee.initCause(e);
			throw ee;
		}
	}

	public class ObjectDomainComparator implements Comparator<ObjectDomain> {
		public int compare(ObjectDomain o1, ObjectDomain o2){
			int result = 0;

			if(o1.getRevision() < o2.getRevision()){
				result = -1;
			}else if(o1.getRevision() > o2.getRevision()){
				result = 1;
			}else{
				result = 0;
			}

			return result;
		}
	}

	/**
	 * オブジェクトのlatestフラグを権限チェックなしに設定を行います。
	 *
	 * @param objectDomain latestフラグを変える対象のオブジェクト
	 * @param latestFlag latestフラグの値
	 * @throws Exception
	 */
	public static void setLatestWithNoCheck(ObjectDomain objectDomain, boolean latestFlag) throws Exception {

		try {
			// レビジョンアップ
			ApplicationContext context = ApplicationContextLoader.getApplicationContext();
			ObjectDao objectDao = (ObjectDao) context.getBean("objectDaoForUtil");
			objectDao.updateLatest(objectDomain, latestFlag);
		} catch (jp.co.ctc_g.eim.framework2.common.exception.EIMException e) {
			// Daoの例外をUtilの例外に変換
			EIMException ee = new EIMException(e.getMessageKey());
			ee.initCause(e);
			throw ee;
		}

	}

	/**
	 * オブジェクトを最新履歴として設定します。
	 * @param objectDomain
	 * @throws Exception
	 */
	public static void setLatest(ObjectDomain objectDomain) throws Exception {

		if (objectDomain == null) {
			// 引数objectDomainとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.VALUE.ILLEGAL");
		}

		try {
			ApplicationContext context = ApplicationContextLoader.getApplicationContext();
			ObjectDao objectDao = (ObjectDao) context.getBean("objectDaoForUtil");
			// 最新履歴フラグ設定
			objectDao.setLatest(objectDomain);
		} catch (jp.co.ctc_g.eim.framework2.common.exception.EIMException e) {
			// Daoの例外をUtilの例外に変換
			EIMException ee = new EIMException(e.getMessageKey());
			ee.initCause(e);
			throw ee;
		}
	}

	/**
	 * オブジェクトのバージョンIDを取得します。
	 * @param object
	 * @return
	 * @throws Exception
	 */
	public static Long getVersionId(ObjectDomain object) throws Exception {
		try {
			// レビジョングループID取得
			ApplicationContext context = ApplicationContextLoader.getApplicationContext();
			ObjectDao objectDao = (ObjectDao) context.getBean("objectDaoForUtilForGetVersion");
			ObjectDomain objectDomain = objectDao.getById(object.getId());
			if (objectDomain == null) {
				// オブジェクトが取得できません。
				throw new EIMException("EIM.ERROR.OBJECT.NOTFOUND");
			}

			Long versionId = object.getRevisionGroupId();

			return versionId;
		} catch (jp.co.ctc_g.eim.framework2.common.exception.EIMException e) {
			// Daoの例外をUtilの例外に変換
			EIMException ee = new EIMException(e.getMessageKey());
			ee.initCause(e);
			throw ee;
		}
	}

}
