package common.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import app.document.search.EIMDocSearchType;
import common.bo.TagTreeItem;
import eim.bo.EIMAccessRole;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMRelation;
import eim.bo.EIMRelationType;
import eim.bo.EIMVersion;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.ObjectUtils;
import jp.co.ctc_g.eim.search.core.updateNotice.business.domain.UpdateNoticeDomain;
import jp.co.ctc_g.eim.search.core.updateNotice.util.UpdateNoticeUtils;




/**
 * 検索フレームワーク連携用クラス
 * 
 */
public class AppUpdateNoticeUtils {
	
	
	/**
	 * 検索フレームワーク連携の有無を判定する。
	 */
	public static boolean doEntry() throws Exception
	{
		return EIMConfig.getValue("SEARCHFW_UPDATE_NOTICE").equals("true");
	}

	/**
	 * 処理種別キーの設定が有効か無効かの判定。
	 * (ドキュメント管理で '-' の際は更新通知しない。 '-' は固定。)
	 * @return true:有効 false:無効
	 */
	public static boolean isEnabledDataType(String settingKey) throws Exception
	{
		boolean flag = false;
		
		if ( doEntry() == true && settingKey != null ) {
			String dataType = EIMConfig.getValue(settingKey);
			if( dataType != null  && !dataType.equals("-") ) 
			{
				flag = true;
			}
		}
		return flag;
	}

	/**
	 * 削除通知情報の登録API
	 *  検索フレームワーク連携がされていれば、削除通知情報を登録する。
	 * 
	 * @param	objectId	登録対象オブジェクトID
	 * @param	settingKey	処理種別キー
	 */
	public static void updateNoticeDelete(long objectId, String settingKey) throws Exception
	{
		if ( doEntry() == true && settingKey != null ) {
			String[] dataTypes = EIMConfig.getValue(settingKey).split(",");
			for (int i = 0; i < dataTypes.length; i++)
			{
				delete(new UpdateNoticeDomain(Long.toString(objectId), dataTypes[i]));
			}

		}
	}

	/**
	 * 更新通知情報の登録API
	 *  検索フレームワーク連携がされていれば、更新通知情報を登録する。
	 * 
	 * @param	objectId	登録対象オブジェクトID
	 * @param	settingKey	処理種別キー
	 */
	public static void updateNoticeInsert(long objectId, String settingKey) throws Exception
	{
		if ( doEntry() == true && settingKey != null )
		{
			String[] dataTypes = EIMConfig.getValue(settingKey).split(",");
			for (int i = 0; i < dataTypes.length; i++)
			{
				entry(new UpdateNoticeDomain(Long.toString(objectId), dataTypes[i]));
			}
		}
	}

	/**
	 * 指定オブジェクトに所属する配下のフォルダ、ドキュメント、タグ、ドキュメントリンクに対して更新通知APIを再帰的に呼び出す。
	 * 
	 * @param sess
	 * @param object	指定のオブジェクト(フォルダorワークスペース)
	 * @param settingKeyFolder	配下オブジェクトがフォルダの場合の処理種別キー
	 * @param settingKeyDoc		配下オブジェクトがドキュメントの場合の処理種別キー
	 * @param settingKeyTag		配下オブジェクトがタグの場合の処理種別キー
	 * @param settingKeyDocLink	配下オブジェクトがドキュメントリンクの場合の処理種別キー
	 */
	public static void updateNoticeInsertChildRecursive(EIMSession sess, EIMObject object, String settingKeyFolder, 
			String settingKeyDoc, String settingKeyTag, String settingKeyDocLink) throws Exception
	{
		updateNoticeInsertChildRecursive(sess, object, settingKeyFolder, settingKeyDoc, settingKeyTag, settingKeyDocLink, null);
	}

	/**
	 * 指定オブジェクトに所属する配下のフォルダ、ドキュメント、タグ、ドキュメントリンクに対して更新通知APIを再帰的に呼び出す。
	 * 
	 * @param sess
	 * @param object	指定のオブジェクト(フォルダorワークスペース)
	 * @param settingKeyFolder	配下オブジェクトがフォルダの場合の処理種別キー
	 * @param settingKeyDoc		配下オブジェクトがドキュメントの場合の処理種別キー
	 * @param settingKeyTag		配下オブジェクトがタグの場合の処理種別キー
	 * @param settingKeyDocLink	配下オブジェクトがドキュメントリンクの場合の処理種別キー
	 * @param conditionHelper	条件判定ヘルパーインスタンス
	 */
	public static void updateNoticeInsertChildRecursive(EIMSession sess, EIMObject object, String settingKeyFolder, 
			String settingKeyDoc, String settingKeyTag, String settingKeyDocLink, AppObjectConditionHelper conditionHelper) throws Exception
	{
		if( doEntry()==false || object==null )
		{
			return;
		}
		
		// 全てが無効なデータタイプの場合は処理しない
		if( !isEnabledDataType(settingKeyFolder) && !isEnabledDataType(settingKeyDoc) && !isEnabledDataType(settingKeyTag) && !isEnabledDataType(settingKeyDocLink) )
		{
			return;
		}
		
		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = conditionHelper;
		if (helper == null) {
			helper = new AppObjectConditionHelper(sess);
		}

		// 対象がフォルダ、ワークスペース以外ならば何もしない
		EIMObjectType objectType = object.getType();
		if(! (helper.isTypeOfFolder(objectType) || helper.isTypeOfWorkspace(objectType)) )
		{
			return;
		}
		
		// リレーションタイプ「ドキュメント」、「リンク」のオブジェクトを再帰的に検索する。
		List<EIMRelation> docRelList = AppSearchUtils.searchRelationByConditionMaker(sess, EIMDocSearchType.SEARCH_ALL_CHILD, EIMAccessRole.NONE, null, object);

		// 子オブジェクト出力ループ
		for (Iterator<EIMRelation> i = docRelList.iterator(); i.hasNext(); ) {

			// 子オブジェクト取得
			EIMRelation rel = (EIMRelation)i.next();
			EIMObject childObj = rel.getChild();
			EIMObjectType objType = childObj.getType();
			
			if(helper.isTypeOfFolder(objType))
			{
				// フォルダ
				insertFolderRecursive(sess, childObj, settingKeyFolder, settingKeyDoc, settingKeyTag, settingKeyDocLink, helper);
				AppUpdateNoticeUtils.updateNoticeInsert(childObj.getId(), settingKeyFolder);
			}
			else if(helper.isTypeOfDocument(objType))
			{
				EIMObject parentObj = rel.getParent();
				if(helper.isDocumentLink(parentObj, childObj))
				{
					// ドキュメントリンク
					AppUpdateNoticeUtils.updateNoticeInsert(childObj.getId(), settingKeyDocLink);
				}
				else
				{
					// ドキュメント
					AppUpdateNoticeUtils.updateNoticeInsert(childObj.getId(), settingKeyDoc);
				}
			}
			else if(helper.isTypeOfTag(objType))
			{
				// タグ
				AppUpdateNoticeUtils.updateNoticeInsert(childObj.getId(), settingKeyTag);
			}
		}
		return;
	}

	/**
	 * 指定された処理種別キーの設定に従って更新通知APIを呼び出す。
	 * 
	 * @param	object		登録対象オブジェクト
	 * @param	settingKey	処理種別キー
	 * @param	allRevision	trueの場合、全リビジョンのオブジェクトを対象とする
	 */
	public static void updateNoticeInsertObject(EIMSession sess, EIMObject object, String settingKey,
			boolean allRevision) throws Exception {
		updateNoticeInsertObject(sess, object, settingKey, allRevision, null);
	}

	/**
	 * 指定された処理種別キーの設定に従って更新通知APIを呼び出す。
	 * 
	 * @param	object		登録対象オブジェクト
	 * @param	settingKey	処理種別キー
	 * @param	allRevision	trueの場合、全リビジョンのオブジェクトを対象とする
	 * @param conditionHelper	条件判定ヘルパーインスタンス
	 */
	public static void updateNoticeInsertObject(EIMSession sess, EIMObject object, String settingKey,
			boolean allRevision, AppObjectConditionHelper conditionHelper) throws Exception {
		if ( doEntry() == true && settingKey != null && isEnabledDataType(settingKey))
		{
			// 条件判定ヘルパー作成
			AppObjectConditionHelper helper = conditionHelper;
			if (helper == null) {
				helper = new AppObjectConditionHelper(sess);
			}

			if (allRevision & object.getRevision() > 0) {

				EIMVersion version = helper.getVersion(object);
				@SuppressWarnings("unchecked")
				List<EIMObject> revisionList = version.getList();
				for (EIMObject revision: revisionList) {
					AppUpdateNoticeUtils.updateNoticeInsert(revision.getId(), settingKey);
				}
			}
			else {
				updateNoticeInsert(object.getId(), settingKey);
			}
		}
	}
	
	/**
	 * 対象オブジェクトがどのオブジェクトタイプか判定をして更新通知APIを呼び出す。
	 * 
	 * @param sess	
	 * @param object	対象オブジェクト
	 * @param settingKeyFolder	対象がフォルダの場合の処理種別キー
	 * @param settingKeyWorkspace	対象がドキュメントの場合の処理種別キー
	 * @param settingKeyTag		対象がタグの場合の処理種別キー
	 * @param settingKeyDocLink	対象がドキュメントリンクの場合の処理種別キー
	 */
	public static void updateNoticeInsertObject(EIMSession sess, EIMObject object, String settingKeyFolder, 
			String settingKeyDoc, String settingKeyTag, String settingKeyWorkspace) throws Exception
	{
		updateNoticeInsertObject(sess, object, 
				settingKeyFolder, settingKeyDoc, settingKeyTag, settingKeyWorkspace, false);
	}

	/**
	 * 対象オブジェクトがどのオブジェクトタイプか判定をして更新通知APIを呼び出す。
	 * 
	 * @param sess	
	 * @param object	対象オブジェクト
	 * @param settingKeyFolder	対象がフォルダの場合の処理種別キー
	 * @param settingKeyWorkspace	対象がドキュメントの場合の処理種別キー
	 * @param settingKeyTag		対象がタグの場合の処理種別キー
	 * @param settingKeyDocLink	対象がドキュメントリンクの場合の処理種別キー
	 * @param allRevision		trueの場合、全リビジョンのオブジェクトを対象とする（ドキュメントのみ）
	 */
	public static void updateNoticeInsertObject(EIMSession sess, EIMObject object, String settingKeyFolder, 
			String settingKeyDoc, String settingKeyTag, String settingKeyWorkspace, boolean allRevision) throws Exception
	{
		updateNoticeInsertObject(sess, object, settingKeyFolder, settingKeyDoc, settingKeyTag, settingKeyWorkspace, allRevision, null);
	}

	/**
	 * 対象オブジェクトがどのオブジェクトタイプか判定をして更新通知APIを呼び出す。
	 * 
	 * @param sess	
	 * @param object	対象オブジェクト
	 * @param settingKeyFolder	対象がフォルダの場合の処理種別キー
	 * @param settingKeyWorkspace	対象がドキュメントの場合の処理種別キー
	 * @param settingKeyTag		対象がタグの場合の処理種別キー
	 * @param settingKeyDocLink	対象がドキュメントリンクの場合の処理種別キー
	 * @param allRevision		trueの場合、全リビジョンのオブジェクトを対象とする（ドキュメントのみ）
	 * @param conditionHelper	条件判定ヘルパーインスタンス
	 */
	public static void updateNoticeInsertObject(EIMSession sess, EIMObject object, String settingKeyFolder, 
			String settingKeyDoc, String settingKeyTag, String settingKeyWorkspace, boolean allRevision, AppObjectConditionHelper conditionHelper) throws Exception
	{
		if( doEntry()==false || object==null )
		{
			return;
		}
		
		// 全てが無効なデータタイプの場合は処理しない
		if( !isEnabledDataType(settingKeyFolder) && !isEnabledDataType(settingKeyDoc) && !isEnabledDataType(settingKeyTag) && !isEnabledDataType(settingKeyWorkspace) )
		{
			return;
		}
		
		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = conditionHelper;
		if (helper == null) {
			helper = new AppObjectConditionHelper(sess);
		}

		EIMObjectType objType = object.getType();

		if(helper.isTypeOfFolder(objType))
		{
			// フォルダ
			AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), settingKeyFolder);
		}
		else if(helper.isTypeOfDocument(objType))
		{
			if( isEnabledDataType(settingKeyDoc) )
			{
				// ドキュメント
				if (allRevision && object.getRev() > 0) {
					EIMVersion version = helper.getVersion(object);
					@SuppressWarnings("unchecked")
					List<EIMObject> revisionList = version.getList();
					for (EIMObject revision: revisionList) {
						AppUpdateNoticeUtils.updateNoticeInsert(revision.getId(), settingKeyDoc);
					}
				}
				else {
					AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), settingKeyDoc);
				}
			}
		}
		else if(helper.isTypeOfTag(objType))
		{
			// タグ
			AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), settingKeyTag);
		}
		else if(helper.isTypeOfWorkspace(objType))
		{
			// ワークスペース
			AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), settingKeyWorkspace);
		}
	}	
	
	/**
	 * 対象オブジェクトがどのオブジェクトタイプか判定をして更新通知APIを呼び出す。
	 * 
	 * @param sess	
	 * @param object	対象オブジェクト
	 * @param settingKeyFolder	対象がフォルダの場合の処理種別キー
	 * @param settingKeyWorkspace	対象がドキュメントの場合の処理種別キー
	 * @param settingKeyTag		対象がタグの場合の処理種別キー
	 * @param settingKeyWorkspace	対象がワークスペースの場合の処理種別キー
	 * @param settingKeyDocLink	対象がドキュメントリンクの場合の処理種別キー
	 */
	public static void updateNoticeInsertObjectLink(EIMSession sess, EIMObject object, String settingKeyFolder, 
			String settingKeyDoc, String settingKeyTag, String settingKeyWorkspace, String settingKeyDocLink, boolean isDocumentLink) throws Exception
	{
		if( doEntry()==false || object==null )
		{
			return;
		}
		
		// 全てが無効なデータタイプの場合は処理しない
		if( !isEnabledDataType(settingKeyFolder) && !isEnabledDataType(settingKeyDoc) && 
				!isEnabledDataType(settingKeyTag) && !isEnabledDataType(settingKeyWorkspace)  && !isEnabledDataType(settingKeyDocLink) )
		{
			return;
		}
		
		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		EIMObjectType objType = object.getType();

		if(helper.isTypeOfFolder(objType))
		{
			// フォルダ
			AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), settingKeyFolder);
		}
		else if(helper.isTypeOfDocument(objType))
		{
			if(isDocumentLink)
			{
				// ドキュメントリンク
				AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), settingKeyDocLink);
			}
			else
			{
				// ドキュメント
				AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), settingKeyDoc);
			}
		}
		else if(helper.isTypeOfTag(objType))
		{
			// タグ
			AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), settingKeyTag);
		}
		else if(helper.isTypeOfWorkspace(objType))
		{
			// ワークスペース
			AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), settingKeyWorkspace);
		}
	}	
	
	
	
	/**
	 * 更新通知情報の一括登録API
	 *  検索フレームワーク連携がされていれば、更新通知情報をまとめて登録する。
	 * 
	 * @param	objList	登録対象オブジェクトリスト
	 * @param	settingKey	処理種別キー
	 */
	public static void updateNoticeInsertObjectList(List<EIMObject> objList, String settingKey) throws Exception
	{
		if (doEntry() == true && settingKey != null && objList.size() != 0 && isEnabledDataType(settingKey)) {
			List<UpdateNoticeDomain> updateNoticeList = new ArrayList<UpdateNoticeDomain>();

			String[] dataTypes = EIMConfig.getValue(settingKey).split(",");
			for(EIMObject obj: objList) {
				for (int i = 0; i < dataTypes.length; i++)
				{
					updateNoticeList.add(new UpdateNoticeDomain(Long.toString(obj.getId()), dataTypes[i]));
				}
			}
			entryList(updateNoticeList);			
		}
	}

	/**
	 * 指定のオブジェクトタイプの配下のオブジェクトタイプに対して再帰的に更新通知APIを呼び出す。
	 * 再帰的に。
	 * @param sess
	 * @param objType	指定のオブジェクトタイプ
	 * @param settingKeyObjType	配下オブジェクトタイプの処理種別キー
	 */
	public static void updateNoticeInsertObjTypeChild(EIMSession sess, EIMObjectType objType, String settingKeyObjType) throws Exception
	{
		if( doEntry()==false || objType==null )
		{
			return;
		}
		
		if( !isEnabledDataType(settingKeyObjType) )
		{
			return;
		}
		
		List<EIMObjectType> objTypeList = new ArrayList<EIMObjectType>();
		ObjectUtils.getChildObjectTypeListRecurrently(sess, objType, objTypeList);
		
		for (Iterator<EIMObjectType> i = objTypeList.iterator(); i.hasNext(); ) 
		{
			EIMObjectType childObjType = (EIMObjectType)i.next();
			AppUpdateNoticeUtils.updateNoticeInsert(childObjType.getId(), settingKeyObjType);
		}
		
		return;
	}

	/**
	 * 親オブジェクトに対して、フォルダかワークスペースかタグの判定をして更新通知APIを呼び出す。
	 * 対象オブジェクトの更新通知は含まない。
	 * @param sess	
	 * @param object	親オブジェクト
	 * @param settingKeyFolder	対象がフォルダの場合の処理種別キー
	 * @param settingKeyWorkspace	対象がワークスペースの場合の処理種別キー
	 * @param settingKeyTag		対象がタグの場合の処理種別キー
	 */
	public static void updateNoticeInsertParent(EIMSession sess, EIMObject object, String settingKeyFolder,
			String settingKeyWorkspace, String settingKeyTag) throws Exception {
		updateNoticeInsertParent(sess, object, settingKeyFolder, settingKeyWorkspace, settingKeyTag, null);
	}

	/**
	 * 親オブジェクトに対して、フォルダかワークスペースかタグの判定をして更新通知APIを呼び出す。
	 * 対象オブジェクトの更新通知は含まない。
	 * @param sess	
	 * @param object	親オブジェクト
	 * @param settingKeyFolder	対象がフォルダの場合の処理種別キー
	 * @param settingKeyWorkspace	対象がワークスペースの場合の処理種別キー
	 * @param settingKeyTag		対象がタグの場合の処理種別キー
	 * @param conditionHelper	条件判定ヘルパーインスタンス
	 */
	public static void updateNoticeInsertParent(EIMSession sess, EIMObject object, String settingKeyFolder,
			String settingKeyWorkspace, String settingKeyTag, AppObjectConditionHelper conditionHelper) throws Exception {
		if( doEntry()==false || object==null )
		{
			return;
		}
		
		// 全てが無効なデータタイプの場合は処理しない
		if( !isEnabledDataType(settingKeyFolder) && !isEnabledDataType(settingKeyWorkspace) && !isEnabledDataType(settingKeyTag) )
		{
			return;
		}
		
		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = conditionHelper;
		if (helper == null) {
			helper = new AppObjectConditionHelper(sess);
		}

		//各種タイプ毎に実施
		EIMObjectType objType = object.getType();
		
		if(helper.isTypeOfFolder(objType))
		{
			// フォルダの場合
			AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), settingKeyFolder);
		}
		else if(helper.isTypeOfWorkspace(objType))
		{
			// ワークスペース
			AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), settingKeyWorkspace);
		}
		else if(helper.isTypeOfTag(objType))
		{
			// タグ
			AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), settingKeyTag);
		}
		return;
	}

	/**
	 * 削除対象がドキュメントまたはフォルダの場合、対象または配下のドキュメントのリンク先について通知する
	 * (検索FWで空フォルダかどうかを保持するため通知が必要)
	 * 
	 * @param sess	
	 * @param object	削除対象オブジェクト
	 * @param settingKeyFolder	対象がフォルダの場合の処理種別キー
	 * @param settingKeyWorkspace	対象がワークスペースの場合の処理種別キー
	 * @param settingKeyTag		対象がタグの場合の処理種別キー
	 */
	public static void updateNoticePhysicalDelLinkParent(EIMSession sess, EIMObject object, String settingKeyFolder, String settingKeyWorkspace) throws Exception {
		updateNoticePhysicalDelLinkParent(sess, object, settingKeyFolder, settingKeyWorkspace, null);
	}

	/**
	 * 削除対象がドキュメントまたはフォルダの場合、対象または配下のドキュメントのリンク先について通知する
	 * (検索FWで空フォルダかどうかを保持するため通知が必要)
	 * 
	 * @param sess	
	 * @param object	削除対象オブジェクト
	 * @param settingKeyFolder	対象がフォルダの場合の処理種別キー
	 * @param settingKeyWorkspace	対象がワークスペースの場合の処理種別キー
	 * @param settingKeyTag		対象がタグの場合の処理種別キー
	 * @param conditionHelper	条件判定ヘルパーインスタンス
	 */
	public static void updateNoticePhysicalDelLinkParent(EIMSession sess, EIMObject object, String settingKeyFolder,
			String settingKeyWorkspace, AppObjectConditionHelper conditionHelper) throws Exception {
		if( doEntry()==false || object==null )
		{
			return;
		}
		
		// 全てが無効なデータタイプの場合は処理しない
		if( !isEnabledDataType(settingKeyFolder) && !isEnabledDataType(settingKeyWorkspace) )
		{
			return;
		}

		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = conditionHelper;
		if (helper == null) {
			helper = new AppObjectConditionHelper(sess);
		}

		EIMObjectType objectType = object.getType();
		if(helper.isTypeOfFolder(objectType))
		{
			// フォルダの場合(再帰的)
			linkFolderRecursive(sess, object, settingKeyFolder, settingKeyWorkspace, helper);
		}
		else if(helper.isTypeOfDocument(objectType))
		{
			updateNoticeLinkParent(sess, object, settingKeyFolder, settingKeyWorkspace, helper);
		}

		return;
	}
	

	/**
	 * タグ解除のための更新通知。
	 *  指定のタグに付与されているフォルダ・ドキュメント・タグに対して更新通知API呼び出す。
	 *  指定タグ(object)の更新通知は含まない。
	 * 
	 * @param	sess	セッション
	 * @param	object	タグ
	 * @param	settingKeyFolder	フォルダの場合の処理種別キー
	 * @param	settingKeyDocument	ドキュメントの場合の処理種別キー
	 * @param	settingKeyTag	タグの場合の処理種別キー
	 */
	public static void updateNoticeInsertTagRemove(EIMSession sess, EIMObject tagObj, TagTreeItem tagItems, HashMap<Long, EIMObject> removeTagObjMap) throws Exception
	{
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		if( tagItems != null && tagItems.getTreeItemList() != null ) {
			recursiveWrapper(sess, tagItems.getTreeItemList(), removeTagObjMap, helper);
		}

	}
	
	/**
	 * WFドキュメント、WF付きフォルダ用の更新通知。
	 *  対象がドキュメントの場合は、ドキュメントのみ更新通知API呼び出し。
	 *  対象がWF付きフォルダの場合は、フォルダおよび、配下のドキュメント・フォルダに対して再帰的に更新通知APIを呼び出す。
	 * 
	 * @param	sess	セッション
	 * @param	object	対象オブジェクト
	 * @param	settingKey	処理種別キー
	 */
	public static void updateNoticeInsertWFObject(EIMSession sess, EIMObject object, 
			String settingKeyDocument, String settingKeyWFFolder, String settingKeyChildDocument, String settingKeyChildFolder) throws Exception
	{
		if( doEntry()==false || object==null )
		{
			return;
		}
		
		// 全てが無効なデータタイプの場合は処理しない
		if( !isEnabledDataType(settingKeyDocument) && !isEnabledDataType(settingKeyWFFolder) &&
				 !isEnabledDataType(settingKeyChildDocument) && !isEnabledDataType(settingKeyChildFolder))
		{
			return;
		}
		
		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		EIMObjectType objectType = object.getType();
		
		if(helper.isTypeOfDocument(objectType))
		{
			// 対象のドキュメント
			AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), settingKeyDocument);
		}
		else if (helper.isTypeOfFolder(objectType))
		{
			// 対象のWFフォルダ
			AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), settingKeyWFFolder);
			
			// 子階層は、子のデータタイプが有効な場合のみ処理
			if( isEnabledDataType(settingKeyChildDocument) || isEnabledDataType(settingKeyChildFolder))
			{
				
				// 子階層取得
				List<EIMObject> objectList = AppObjectUtil.getChildEIMObjectRecurrently(sess, object, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
				
				for(EIMObject childObj: objectList) {
	
					EIMObjectType childObjType = childObj.getType();
					
					if(helper.isTypeOfFolder(childObjType))
					{
						// WFフォルダ配下のフォルダ
						AppUpdateNoticeUtils.updateNoticeInsert(childObj.getId(), settingKeyChildFolder);
					}
					else if (helper.isTypeOfDocument(childObjType))
					{
						// WFフォルダ配下のドキュメント
						AppUpdateNoticeUtils.updateNoticeInsert(childObj.getId(), settingKeyChildDocument);
					}
				}
			}
		}
		return;
	}

	/**
	 * 対象オブジェクトがブランチドキュメントの場合、ブランチ元を通知対象とする。
	 *  対象オブジェクトは通知しない。
	 * 
	 * @param sess	
	 * @param object	対象オブジェクト
	 * @param settingKeyFolder	対象がフォルダの場合の処理種別キー
	 * @param insertFlag	true：更新通知、false；削除通知
	 */
	public static void updateNoticeInsertBranch(EIMSession sess, EIMObject object, String settingKeyOrgBranch) throws Exception {
		updateNoticeInsertBranch(sess, object, settingKeyOrgBranch, null);
	}

	/**
	 * 対象オブジェクトがブランチドキュメントの場合、ブランチ元を通知対象とする。
	 *  対象オブジェクトは通知しない。
	 * 
	 * @param sess	
	 * @param object	対象オブジェクト
	 * @param settingKeyFolder	対象がフォルダの場合の処理種別キー
	 * @param insertFlag	true：更新通知、false；削除通知
	 * @param conditionHelper	条件判定ヘルパーインスタンス
	 */
	public static void updateNoticeInsertBranch(EIMSession sess,
			EIMObject object, String settingKeyOrgBranch, AppObjectConditionHelper conditionHelper) throws Exception {
		if( doEntry()==false || object==null )
		{
			return;
		}
		
		// 無効なデータタイプの場合は処理しない
		if( !isEnabledDataType(settingKeyOrgBranch) )
		{
			return;
		}

		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = conditionHelper;
		if (helper == null) {
			helper = new AppObjectConditionHelper(sess);
		}

		EIMObjectType objType = object.getType();

		if(helper.isTypeOfDocument(objType))
		{
			EIMRelationType relTypeBranch = helper.getRelationTypeOfBranch();
			List<EIMRelation> relationList = helper.getParentRelationListByRelType(object, relTypeBranch, EIMAccessRole.NONE);

			// ブランチ元は1つのはず
			for(EIMRelation rel : relationList)
			{
				EIMObject obj = rel.getParent();
				AppUpdateNoticeUtils.updateNoticeInsert(obj.getId(), settingKeyOrgBranch);
			}
		}
		return;
	}	
	
	/**
	 * 指定オブジェクトに所属する配下のフォルダ、ドキュメント、タグ、ドキュメントリンクに対して更新通知APIを再帰的に呼び出す。
	 * 
	 * @param sess
	 * @param object	指定のオブジェクト(フォルダorワークスペース)
	 * @param settingKeyFolder	配下オブジェクトがフォルダの場合の処理種別キー
	 * @param settingKeyDoc		配下オブジェクトがドキュメントの場合の処理種別キー
	 * @param settingKeyTag		配下オブジェクトがタグの場合の処理種別キー
	 * @param settingKeyDocLink	配下オブジェクトがドキュメントリンクの場合の処理種別キー
	 */
	private static void insertFolderRecursive(EIMSession sess, EIMObject object, String settingKeyFolder, 
			String settingKeyDoc, String settingKeyTag, String settingKeyDocLink, AppObjectConditionHelper helper) throws Exception
	{
		if( doEntry()==false || object==null )
		{
			return;
		}
		
		// リレーションタイプ「ドキュメント」、「リンク」のオブジェクトを検索する。
		List<EIMRelation> docRelList = AppSearchUtils.searchRelationByConditionMaker(sess, EIMDocSearchType.SEARCH_ALL_CHILD, EIMAccessRole.NONE, null, object);

		// 子オブジェクト出力ループ
		for (Iterator<EIMRelation> i = docRelList.iterator(); i.hasNext(); ) {

			// 子オブジェクト取得
			EIMRelation rel = (EIMRelation)i.next();
			EIMObject childObj = rel.getChild();
			EIMObjectType objType = childObj.getType();
			
			if(helper.isTypeOfFolder(objType))
			{
				insertFolderRecursive(sess, childObj, settingKeyFolder, settingKeyDoc, settingKeyTag, settingKeyDocLink, helper);
				// フォルダ
				AppUpdateNoticeUtils.updateNoticeInsert(childObj.getId(), settingKeyFolder);
			}
			else if(helper.isTypeOfDocument(objType))
			{
				EIMObject parentObj = rel.getParent();
				if(helper.isDocumentLink(parentObj, childObj))
				{
					// ドキュメントリンク
					AppUpdateNoticeUtils.updateNoticeInsert(childObj.getId(), settingKeyDocLink);
				}
				else
				{
					// ドキュメント
					AppUpdateNoticeUtils.updateNoticeInsert(childObj.getId(), settingKeyDoc);
				}
			}
			else if(helper.isTypeOfTag(objType))
			{
				// タグ
				AppUpdateNoticeUtils.updateNoticeInsert(childObj.getId(), settingKeyTag);
			}
		}
	}
	
	/**
	 * 削除対象がドキュメントまたはフォルダの場合、対象または配下のドキュメントのリンク先について通知する
	 *  フォルダの場合は配下のフォルダまで再帰的に通知する。
	 * 
	 * @param sess	
	 * @param object	削除対象オブジェクト
	 * @param settingKeyFolder	対象がフォルダの場合の処理種別キー
	 * @param settingKeyWorkspace	対象がワークスペースの場合の処理種別キー
	 * @param helper	helper
	 */
	private static void linkFolderRecursive(EIMSession sess, EIMObject object, String settingKeyFolder, String settingKeyWorkspace, AppObjectConditionHelper helper) throws Exception
	{
		// リレーションタイプ「ドキュメント」のオブジェクトを検索する。
		List<EIMRelation> docRelList = AppSearchUtils.searchRelationByConditionMaker(sess, EIMDocSearchType.SEARCH_CHILD, EIMAccessRole.NONE, null, object);

		// 子オブジェクト出力ループ
		for (Iterator<EIMRelation> i = docRelList.iterator(); i.hasNext(); ) {

			// 子オブジェクト取得
			EIMRelation rel = (EIMRelation)i.next();
			EIMObject childObj = rel.getChild();
			EIMObjectType childObjType = childObj.getType();
			
			if(helper.isTypeOfFolder(childObjType))
			{
				// フォルダ
				linkFolderRecursive(sess, childObj, settingKeyFolder, settingKeyWorkspace, helper);
			}
			else if(helper.isTypeOfDocument(childObjType))
			{
				updateNoticeLinkParent(sess, childObj, settingKeyFolder, settingKeyWorkspace, helper);
			}
		}
		return;
	}
	
	
	/**
	 * 全てのリンク先について削除通知を行う。
	 * 
	 * @param sess	
	 * @param object	削除対象ドキュメント
	 * @param settingKeyFolder	対象がフォルダの場合の処理種別キー
	 * @param settingKeyWorkspace	対象がワークスペースの場合の処理種別キー
	 * @param helper	helper
	 */
	private static void updateNoticeLinkParent(EIMSession sess, EIMObject object, String settingKeyFolder, String settingKeyWorkspace, AppObjectConditionHelper helper) throws Exception
	{
		// ドキュメント
		// 全てのリンク先を通知
		long[] linkArray = AppObjectUtil.getIntAttrs(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_TARGET_TO_LINK"));
		if( linkArray!= null )
		{
			for(long parentId : linkArray)
			{
				EIMObject parent = ObjectUtils.getObjectById(sess, parentId);
				if( parent != null )
				{
					EIMObjectType parentObjType = parent.getType();
					if(helper.isTypeOfFolder(parentObjType))
					{
						// フォルダ
						updateNoticeInsert(parentId, settingKeyFolder);
						
					}
					else if(helper.isTypeOfWorkspace(parentObjType))
					{
						// ワークスペース
						updateNoticeInsert(parentId, settingKeyWorkspace);
					}
				}
			}
		}
		return;
	}
	
	private static void recursiveWrapper(EIMSession sess, List tagChildObjs, HashMap<Long, EIMObject> removeTagObjMap, AppObjectConditionHelper helper) throws Exception
	{
		Iterator i = tagChildObjs.iterator();
		for (; i.hasNext(); ) {
			TagTreeItem childItem = (TagTreeItem)i.next();
			EIMObject object = childItem.getEimObject();

			// フォルダの場合はTagTreeItem._treeItemListについて再帰処理
			if( helper.isTypeOfFolder(object.getType()) )
			{
				recursive(sess, childItem.getTreeItemList(), removeTagObjMap, helper);
			}

			EIMObject target = removeTagObjMap.get(object.getId());
			if( target!=null )
			{
				updateNoticeInsertObject(sess, object, 
						"SEARCHFW_SELECTTAG_ALLOCATE_FOLDER",
						"SEARCHFW_SELECTTAG_ALLOCATE_DOCUMENT",
						"SEARCHFW_SELECTTAG_ALLOCATE_TAG", null);
			}
		}
		
	}

	
	private static void recursive(EIMSession sess, List tagChildObjs, HashMap<Long, EIMObject> removeTagObjMap, AppObjectConditionHelper helper) throws Exception
	{
		Iterator i = tagChildObjs.iterator();
		for (; i.hasNext(); ) {
			TagTreeItem childItem = (TagTreeItem)i.next();
			EIMObject object = childItem.getEimObject();

			// フォルダの場合はTagTreeItem._treeItemListについて再帰処理
			if( helper.isTypeOfFolder(object.getType()) )
			{
				recursive(sess, childItem.getTreeItemList(), removeTagObjMap, helper);
			}
			
			EIMObject target = removeTagObjMap.get(object.getId());
			if( target!=null )
			{
				// ドキュメントリンクは、フォルダ配下のものでもタグ直下扱いされるのでリンクがここを通ることはないので
				// リンクフラグは固定でfalse
				updateNoticeInsertObjectLink(sess, object, 
						"SEARCHFW_SELECTTAG_CHILD_FOLDER",
						"SEARCHFW_SELECTTAG_CHILD_DOCUMENT",
						"SEARCHFW_SELECTTAG_CHILD_TAG",
						null,
						"SEARCHFW_SELECTTAG_CHILD_DOCUMENTLINK",
						false);
			}
		}
		
	}
		
	/**
	 * 更新通知ユーティリティの登録更新処理を呼び出す。
	 */
	private static void entry(UpdateNoticeDomain updateNotice) throws Exception
	{
		UpdateNoticeUtils.insertCreateUpdate(updateNotice);
	}

	/**
	 * 更新通知ユーティリティの削除処理を呼び出す。
	 */
	private static void delete(UpdateNoticeDomain updateNotice) throws Exception
	{
		UpdateNoticeUtils.insertDelete(updateNotice);
	}

	/**
	 * 更新通知ユーティリティの一括登録更新処理を呼び出す。
	 */
	private static void entryList(List<UpdateNoticeDomain> updateNoticeList) throws Exception
	{
		UpdateNoticeUtils.insertCreateUpdateList(updateNoticeList);
	}
}
