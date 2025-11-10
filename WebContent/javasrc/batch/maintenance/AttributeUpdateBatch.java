package batch.maintenance;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.sql.DataSource;

import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.context.ApplicationContext;

import common.bo.AttributeUpdater;
import common.bo.AttributeUpdaterItem;
import common.util.AppConstant;
import common.util.AppMessageUtils;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectUtil;
import common.util.AttributeUtil;

import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMRelation;
import eim.bo.EIMRelationType;
import eim.bo.EIMResource;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.RelationUtils;

/**
*
* 属性一括更新バッチ
*
*/
public class AttributeUpdateBatch {
	
	/** ログ */
	private static Log log = LogFactory.getLog(AttributeUpdateBatch.class);
	
	/** アプリケーションコンテキスト */
	private static ApplicationContext context;
	
	/** セッション */
	private static EIMSession sess;
	
	/** 更新対象フォルダ */
	private static EIMObject targetFolder;
	
	/** 条件判定ヘルパー */
	private static AppObjectConditionHelper helper;
	
	/**
	 * 属性一括更新バッチメイン処理
	 * @param args なし
	 */
	public static void main(String[] args) {
		try {
			// 開始ログ
			log.info("-------------属性更新バッチ開始-------------");
			
			// 初期処理
			init();
			
			// 更新対象フォルダチェック
			if(targetFolder == null){
				log.error(" 属性更新対象フォルダが取得できません。設定ファイルのUPDATE_ATTR_FOLDER_IDを確認してください。");
				return ;
			}
			if(!helper.isTypeOfFolder(targetFolder.getType()) && !helper.isTypeOfWorkspace(targetFolder.getType())){
				log.error(" 属性更新対象フォルダがフォルダ、ワークスペースタイプではありません。設定ファイルのUPDATE_ATTR_FOLDER_IDを確認してください。");
				return ;
			}
			
			log.info("[更新対象フォルダ]"+targetFolder.getId()+" : "+targetFolder.getName());
			
			// 対象のフォルダタイプに紐付く属性を取得
			List<EIMAttribute> attributeList = targetFolder.getAttributeList();
			HashMap<Long,EIMAttribute> attributeMap = new HashMap<Long,EIMAttribute>();
			List<String> systemAttList = Arrays.asList(AppConstant.SYSTEM_ATTRIBUTE_DEFNAME);
			log.info("[更新対象属性タイプ]");
			for(EIMAttribute attr : attributeList){
				if (systemAttList.contains(attr.getType().getDefName())) {
					continue;
				}
				// 有効期限は除く
				if(attr.getType().getDefName().equals("有効期限")){
					continue;
				}
				long attrTypeId = attr.getType().getId();
				log.info(" "+attr.getType().getId()+","+attr.getType().getDefName());
				attributeMap.put(attrTypeId, attr);
			}
			
			// 指定したフォルダ配下のドキュメント全て取得する
			List<EIMObject> objectList = AppObjectUtil.getChildEIMObjectRecurrently(sess, targetFolder, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
			int index = 0;
			// 指定したフォルダ配下のドキュメントの属性を更新する
			log.info( "[属性対象ドキュメント]");
			for (EIMObject obj :objectList){
				if(helper.isTypeOfDocument(obj.getType())){
					log.info(" "+obj.getId()+","+obj.getName());
					updateAttribute(obj,attributeMap);
					index++;
				}
			}
			sess.commit();
			log.info( "[属性更新処理実行ドキュメント数]"+index);
			
		} catch(EIMException eime) {
			log.error(AppMessageUtils.makeLogMessage(1,eime.getMessage()), eime);
			try{
				// ロールバック
				if(sess != null){
					sess.rollback();
				}
				
			} catch (Exception se) {
				log.error(AppMessageUtils.makeLogMessage(1,se.getMessage()), se);
				
			}
			
		} catch (Exception e) {
			log.error(AppMessageUtils.makeLogMessage(1,e.getMessage()), e);
			try{
				// ロールバック
				if(sess != null){
					sess.rollback();
				}
				
			} catch (Exception se) {
				log.error(AppMessageUtils.makeLogMessage(1,se.getMessage()), se);
				
			}
			
		} finally {
			try{
				log.info("-------------属性更新バッチ終了-------------");
				// セッションクローズ
				EIMThreadContext.removeEIMSession();
				if(sess != null){
					sess.close();
				}
				
			} catch (Exception se) {
				log.error(AppMessageUtils.makeLogMessage(1,se.getMessage()), se);
				
			}
		}
		
	}
	
	/**
	 * 初期処理
	 * @throws Exception 例外
	 */
	private static void init() throws Exception{
		
		//user取得
		EIMUser user = new EIMUser(1, null, null, null, null, null, 255, 0, null);
		//lang取得
		String lang = "";
		String EIM_CONFIG_LANG = "MESSAGELANG";
		String DEFAULT_LANG	= "JA";
		if(EIMConfig.get(EIM_CONFIG_LANG) != null){
			lang = EIMConfig.get(EIM_CONFIG_LANG);
		}else{
			lang = DEFAULT_LANG;
		}
		//Session
		context = ApplicationContextLoader.getContext();
		DataSource ds = (DataSource)context.getBean("dataSource");
		sess = new EIMSession(user,lang);
		sess.setConnection(ds.getConnection());
		sess.setConsoleMode(true);
		sess.getDBConnection().setAutoCommit(false);
		EIMThreadContext.putEIMSession(sess);
		
		// 属性更新対象フォルダID設定
		if(EIMConfig.get("UPDATE_ATTR_FOLDER_ID") != null){
			int targetFolderId = Integer.parseInt(EIMConfig.get("UPDATE_ATTR_FOLDER_ID"));
			targetFolder = ObjectUtils.getObjectById(sess, targetFolderId);
		}
		// 条件判定ヘルパー
		helper = new AppObjectConditionHelper(sess);
	}
	
	/**
	 * 属性更新
	 * @param document
	 * @param attrMap
	 * @throws Exception
	 */
	private static void updateAttribute(EIMObject object, HashMap<Long,EIMAttribute> attrMap)throws Exception{
		EIMObjectType objType = ObjectUtils.getObjectTypeById(sess, object.getType().getId());
		List<EIMAttributeType> attrTypeList = ObjectAttributeUtils.getAttributeTypeList(sess, objType);
		//List<AttributeUpdaterItem> attUpdateItemList = new ArrayList<AttributeUpdaterItem>();
		for(EIMAttributeType attrType : attrTypeList){
			long attrTypeId = attrType.getId();
			//log.info("    --debug:属性更新タイプチェック："+attrTypeId+","+attrType.getDefName());
			if(attrMap.get(attrTypeId) != null){
				EIMAttribute attr = attrMap.get(attrTypeId);
				AppObjectUtil.setAttr(sess, object, attr);
				//log.info("   --属性更新成功："+attr.getType().getDefName());
			}
		}
	}
}
