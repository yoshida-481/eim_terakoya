package jp.co.ctc_g.eim.framework.search.impl;

import java.util.ArrayList;
import java.util.List;

import org.springframework.context.ApplicationContext;

import eim.bo.EIMSearchConditionGroup;
import eim.net.EIMSession;
import jp.co.ctc_g.eim.framework.common.exception.EIMAppException;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

public class SearchConditionPlugInManager {
	
	/** SearchConditionPlugInManagerの実体 */
	private static SearchConditionPlugInManager _instance;
	
	private ObjectSearchConditionPlugInImpl objectSerchCondition;
	
	private RelationSearchConditionPlugInImpl relationSearchCondition;
	
	/**
	 * コンストラクタ
	 * @throws Exception 
	 */
	private SearchConditionPlugInManager() throws Exception {
		ApplicationContext contxt = ApplicationContextLoader.getContext();
		EIMSession sess = EIMThreadContext.getEIMSession();
		
		List<SearchConditionBase> conditionList = new ArrayList<SearchConditionBase>();
		// オブジェクトサーチ用の情報を取得
		try {
			objectSerchCondition = (ObjectSearchConditionPlugInImpl) contxt.getBean("ObjectSearchConditionPlugin");
			if (objectSerchCondition != null) {
				conditionList.addAll(objectSerchCondition.getPlugins());
			}
		}catch (Exception e) {
			e.printStackTrace();
			objectSerchCondition = null;
			EIMAppException eae = new EIMAppException(sess, "EIM.ERROR.SYSTEMERROR");
			eae.setStackTrace(e.getStackTrace());
			throw eae;
		}
		
		// リレーションサーチ用の情報を取得
		try {
			relationSearchCondition = (RelationSearchConditionPlugInImpl) contxt.getBean("RelationSearchConditionPlugin");
			if (relationSearchCondition != null) {
				conditionList.addAll(relationSearchCondition.getPlugins());
			}
		}catch (Exception e) {
			e.printStackTrace();
			relationSearchCondition = null;
			EIMAppException eae = new EIMAppException(sess, "EIM.ERROR.SYSTEMERROR");
			eae.setStackTrace(e.getStackTrace());
			throw eae;
		}
		
	}
	
	/**
	 * シングルトンにより実体を取得する
	 * @return  SearchConditionPlugInManagerの実体
	 * @throws Exception 
	 */
	public static synchronized SearchConditionPlugInManager getInstance() throws Exception {
		
		if ( _instance == null ) {
			_instance = new SearchConditionPlugInManager();
		}
		
		return _instance;
	}
	
	/**
	 * 実体を削除する
	 */
	public static void clear() {
		_instance.objectSerchCondition = null;
		_instance.relationSearchCondition = null;
		_instance = null;
	}
	
	/**
	 * オブジェクトサーチの条件を取得
	 * @return
	 */
	public EIMSearchConditionGroup getObjectSearchCondition(long id) throws Exception{
		
		EIMSearchConditionGroup result = null;
		
		if (objectSerchCondition != null) {
			result = objectSerchCondition.getCondition(id);
		}
		
		return result;
	}
	
	/**
	 * リレーションサーチの条件を取得
	 * @return
	 */
	public EIMSearchConditionGroup getRelationSearchCondition(long id) throws Exception{
		
		EIMSearchConditionGroup result = null;
		
		if (relationSearchCondition != null) {
			result = relationSearchCondition.getRelationCondition(id);
		}
		
		return result;
	}
	
	/**
	 * リレーションサーチの親の条件を取得
	 * @return
	 */
	public EIMSearchConditionGroup getParentSearchCondition(long id) throws Exception{
		
		EIMSearchConditionGroup result = null;
		
		if (relationSearchCondition != null) {
			result = relationSearchCondition.getParentCondition(id);
		}
		
		return result;
	}
	
	/**
	 * リレーションサーチの子の条件を取得
	 * @return
	 */
	public EIMSearchConditionGroup getChildSearchCondition(long id) throws Exception{
		
		EIMSearchConditionGroup result = null;
		
		if (relationSearchCondition != null) {
			result = relationSearchCondition.getChildCondition(id);
		}
		
		return result;
	}
	
}