package common.util;

import java.io.File;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import eim.bo.EIMAccessRole;
import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMDirectory;
import eim.bo.EIMException;
import eim.bo.EIMFile;
import eim.bo.EIMFormat;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMRelation;
import eim.bo.EIMRelationType;
import eim.bo.EIMResource;
import eim.bo.EIMSearchConditionCompare;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchConditionIn;
import eim.bo.EIMSearchConditionLike;
import eim.bo.EIMSearchOperatorEnum;
import eim.bo.EIMSearchResultList;
import eim.bo.EIMSearchSelectEIMObject;
import eim.bo.EIMSearchSelectEIMRelation;
import eim.bo.EIMSecurity;
import eim.bo.EIMUser;
import eim.bo.EIMVersion;
import eim.net.EIMSession;
import eim.util.AccessUtils;
import eim.util.AttributeUtils;
import eim.util.DateUtils;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.FileUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.OperationHistoryUtils;
import eim.util.RelationAttributeUtils;
import eim.util.RelationUtils;
import eim.util.SearchUtils;
import eim.util.SecurityUtils;
import eim.util.StringUtils;
import eim.util.TypeConvertUtils;
import eim.util.VersionUtils;
import jp.co.ctc_g.eim.framework.common.exception.EIMSysException;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework2.integration.dao.cache.DataCacheDao;
import jp.co.ctc_g.eim.framework2.integration.dao.plugin.DatabasePlugIn;
import jp.co.ctc_g.eim.framework2.integration.dao.plugin.DatabasePlugInLoader;
import jp.co.ctc_g.eim.framework2.integration.dao.util.DBUtils;

/**
 *
 * (ドキュメント管理用) 業務共通処理クラス
 *
 */
public class AppLogicUtil {

	/** replaceDocLinkPath 関数の戻り値 */
	public static final int REPLACE_DOCLINK_PATH_ERROR = -1;
	/** replaceDocLinkPath 関数の戻り値 */
	public static final int REPLACE_DOCLINK_PATH_ERROR_DOCLINK_MOVED = -2;

	/** SearchFramework 更新通知要処理種別キー ブランチ元ドキュメント*/
	private static String updateNoticeKeyBrachDoc = "SEARCHFW_LOGICDEL_ORG_BRANCH_DOCUMENT";


	/**
	 * PDFアイコンの表示可否判定結果を返します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 表示判定するドキュメントのオブジェクト
	 * @param formatPDF 公開ドキュメントのフォーマット
	 * @return 公開済み、かつ、公開ドキュメントの拡張子が「pdf」ならばtrue。それ以外はfalse。
	 * @throws Exception
	 */
	public 	static boolean isDspPdfIcon(EIMSession sess, EIMObject obj, EIMFormat formatPDF) throws Exception {

		if (obj != null && formatPDF != null) {
			EIMFile filePDF = FileUtils.getFile(sess, obj, formatPDF);
			// 拡張子が「pdf」の場合のみ
			if (filePDF != null && filePDF.getExt().equals(EIMConfig.get("PDF_EXT"))) {
				return true;
			}
		}
		return false;
	}

	/**
	 * PDFアイコンの表示可否判定結果を返します。
	 *
	 * @param file 公開ファイル
	 * @return 公開済み、かつ、公開ファイルの拡張子が「pdf」ならばtrue。それ以外はfalse。
	 */
	public static boolean isDspPdfIcon(FileDomain file) {

		if (file == null || file.getExt() == null) {
			return false;
		}

		// 拡張子が「pdf」の場合のみ
		if (!file.getExt().equals(EIMConfig.get("PDF_EXT"))) {
			return false;
		}
		return true;
	}

	/**
	 * 公開済み判定結果を返します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 表示判定するドキュメントのオブジェクト
	 * @return 公開済みならばtrue。それ以外はfalse。
	 * @throws Exception
	 */
	public static boolean isPublished(EIMSession sess, EIMObject obj) throws Exception {
		if(obj != null) {
			// 公開済みの場合のみ
			if( obj.getStatus() == null ) {
				return false;
			} else if( obj.getStatus().getType().getKind() == AppConstant.STATUS_TYPE_KIND_ID_PUBLIC ) {
				return true;
			}
		}
		return false;
	}

	/**
	 * WFなしドキュメントがチェックアウト可能か否かの判定結果を返します。
	 *
	 * <li>自分が最新、かつ、1つ前のドキュメントの最新履歴フラグが真の場合は不可false)を返します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj チェックアウトするドキュメントのオブジェクト
	 * @return チェックアウト可能か否かの判定結果
	 * @throws Exception
	 */
	public 	static boolean isCheckoutEnabledNoWFDoc(EIMSession sess, EIMObject obj) throws Exception {

		// 最新履歴フラグが真、かつ、ロックユーザがいない、かつ、履歴が複数ある場合
		if (obj.getLatest() && obj.getLockUser() == null && obj.getRev() > 0) {
			// バージョンを取得
			EIMVersion version = VersionUtils.getVersion(sess, obj);
			// 最新から1つ前の履歴のオブジェクトを取得
			EIMObject prevObj = version.getObjectByRev(obj.getRev() - 1);
			// 1つ前のオブジェクトの最新履歴フラグが真の場合(＝まだチェックインしてない)はチェックアウトできない
			if (prevObj.getLatest()) {
				return false;
			}
		}
		return true;
	}

	/**
	 * 最新バージョンのオブジェクトを返します。
	 *
	 * <li>読取権限のみの場合は公開済みの最新バージョンのオブジェクトが返ります。
	 * <li>書込権限がある場合は最新バージョンのオブジェクトが返ります。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param object ドキュメントのオブジェクト
	 * @return 最新バージョンのオブジェクト
	 * @throws Exception
	 */
	public 	static EIMObject getLatestObject(EIMSession sess, EIMObject object) throws Exception {

		//Version
		EIMVersion version = VersionUtils.getVersion(sess, object);

		//Object List
		List objList = version.getList();

		//最新バージョンを取得
		int i = objList.size();
		if(i != 0) {
			object = (EIMObject)objList.get(i-1);
		}
		return object;
	}

	/**
	 * フォルダ系列のオブジェクトタイプIDのHashSetを取得します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @return フォルダ系列のオブジェクトタイプIDのHashSet [key]オブジェクトタイプID
	 * @throws Exception
	 */
	public static HashSet getFolderObjTypeIdSet(EIMSession sess) throws Exception {

		List folderTypeList = new ArrayList();
		// フォルダのオブジェクトタイプのリスト取得
		EIMObjectType folderType = eim.util.ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_FOLDER"));
		ObjectUtils.getChildObjectTypeListRecurrently(sess, folderType, folderTypeList);
		folderTypeList.add(folderType);

		HashSet set = new HashSet();
		for (Iterator iter = folderTypeList.iterator(); iter.hasNext();) {
			set.add(new Long(((EIMObjectType)iter.next()).getId()));
		}
		return set;
	}

	/**
	 * ワークスペースの改名処理を行います。
	 *
	 * <li>Windows禁止文字の場合、例外を返却します。
	 * <li>操作履歴を更新します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param object 対象オブジェクト
	 * @param prmObjName 改名後のオブジェクト名
	 * @throws Exception
	 */
	public static void renameObjectForWorkSpace(EIMSession sess, EIMObject object, String prmObjName) throws Exception {

		// Windows禁止文字チェック
		AppObjectUtil.checkValidateFName(sess, prmObjName);

		//Object Type For Parent Recurrently
		ObjectUtils.getObjectTypeById(sess, object.getType().getId());

		//Relation Type
		EIMRelationType relType = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));

		//Attribute Type
		EIMAttributeType attType = AttributeUtils.getAttributeTypeByName(sess, "パス");

		String oldObjName = object.getName();

		//Rename Object
		if(!prmObjName.equals(object.getName())){
			object = ObjectUtils.rename(sess, object, null, prmObjName, EIMConstant.DEPU_CHECK_TYPE_NAME);
		}

		//子オブジェクトのパスを変更する(改名することで必要となる)
		String path = "/";
		setPath(sess,object,path,oldObjName);

		//Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.UPDATE_WORKSPACE,
				EIMConstant.TARGET_UPDATE, EIMConstant.OBJECT, object,
				null, null, null, null);

	}

	/**
	 * オブジェクトの改名処理を行います。
	 *
	 * <li>Windows禁止文字の場合、例外を返却します。
	 * <li>対象がドキュメントで、かつ、拡張子が無い場合、例外を返却します。
	 * <li>対象がフォルダの場合、子フォルダ/子ドキュメントに対して「パス」の更新処理を行います。
	 * <li>アクセス履歴を更新します。
	 * <li>操作履歴を更新します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param object 対象オブジェクト
	 * @param prmObjName 改名後のオブジェクト名
	 * @param helper 条件判定ヘルパー
	 * @throws Exception
	 */
	public static void renameObject(EIMSession sess, EIMObject object, String prmObjName,
			AppObjectConditionHelper helper) throws Exception {

		EIMRelation docRel = null;

		List relList = RelationUtils.getParentRelationListByRelType(sess, object, RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT")),EIMAccessRole.READ);

		if(!relList.isEmpty()) {
			docRel = (EIMRelation)relList.get(0);
		}

		// ステータスチェック - WF付きフォルダ以下、かつ、編集中以外の場合、改名できない
		if (helper.isUnderFolderWithWorkflow(object)
				&& object.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {
			// 改名権限がありません。
			throw new EIMException(sess, "EIM.ERROR.LOGIC.NORENAMEROLE");
		}

		// ASCII制御文字がある場合空文字列に置き換える
		prmObjName = AppObjectUtil.escapeAsciiControlCode(sess, prmObjName);

		// Windows禁止文字チェック
		AppObjectUtil.checkValidateFName(sess, prmObjName);

		// 拡張子の取得 ('.'付き)
		String fileExt = StringUtils.nullToBlank(StringUtils.getFileExt(prmObjName));

		// フォルダのオブジェクトタイプSetを取得 [key]オブジェクトタイプID
		HashSet folderTypeSet = getFolderObjTypeIdSet(sess);

		//Object Type For Parent Recurrently
		EIMObjectType objType = ObjectUtils.getObjectTypeById(sess, object.getType().getId());

		//Relation Type
		EIMRelationType relType = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));

		//Format
		EIMFormat format = FileUtils.getDefaultFormat(sess, objType);

		String oldObjName = object.getName();

		//Rename Object
		//object = ObjectUtils.rename(sess, object, prmObjName);
		if(!prmObjName.equals(object.getName())){
			object = ObjectUtils.rename(sess, object, docRel, prmObjName, EIMConstant.DEPU_CHECK_NAME_REV);
		}

		// フォルダの場合のみ再帰的に「パス」を更新
		if (folderTypeSet.contains(new Long(objType.getId())))
		{
			String path = StringUtils.nullToBlank(AppObjectUtil.getPath(object));
			setPath(sess,object,path,oldObjName);

		}
		//If Document Then Rename File
		else if(helper.isTypeOfDocument(objType))
		{
			//拡張子が変わる場合はファイルの拡張子も変更
			EIMDirectory dir = format.getDirectory();
			EIMFile file = FileUtils.getFile(sess, object, format);
			boolean isExtChanged = !file.getExt().equals(fileExt);		//拡張子変更判定

			if (isExtChanged) {
				FileUtils.prepareFileAccess(sess, object, file);
				File fromF = new File(dir.getPath() + FileUtils.getFileName(object, file));
				File toF = new File(dir.getPath() + object.getId() + fileExt);
				boolean succeeded = fromF.renameTo(toF);
				if (!succeeded) {
					throw new Exception(EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCMENT", null));
				}
			}

			//Rename File
			FileUtils.renameFile(sess, object, format, prmObjName);

			//公開ドキュメント(PDF化ファイル)の改名
			EIMFormat formatPDF = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));
			EIMFile filePDF = FileUtils.getFile(sess, object, formatPDF);
			if (filePDF != null) {
				FileUtils.renameFile(sess, object, formatPDF,
						StringUtils.getFileBody(prmObjName) + filePDF.getExt());

				//シンボリックリンク且つ拡張子が変わる場合、シンボリックリンクを再作成する。
				FileSystem fs = FileSystems.getDefault();
				File dstFile = new File(filePDF.getDirectory().getPath() + object.getId() + filePDF.getExt());
				boolean isSymbolicLink = Files.isSymbolicLink(fs.getPath(dstFile.getPath()));
				if(isExtChanged && isSymbolicLink) {
					dstFile.delete();
					FileUtils.createSymbolicLink(new File(dir.getPath() + object.getId() + fileExt), dstFile);
				}
			}
		}

		//Access
		AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.CHANGENAME");

		//パス
		String path = StringUtils.nullToBlank(AppObjectUtil.getPath(object));

		//Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.RENAME,
				EIMConstant.TARGET_UPDATE, EIMConstant.OBJECT, object,
				null, null, null, path);
	}

	/**
	 * 子フォルダ/子ドキュメントの「パス」属性を更新します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param object 対象オブジェクト
	 * @param path 対象オブジェクトの「パス」属性値
	 * @param oldObjName 変更前のオブジェクト名
	 * @throws Exception
	 */
	public static void setPath(EIMSession sess, EIMObject object, String path, String oldObjName) throws Exception {
		String newObjName = path + object.getName();
		oldObjName = path + oldObjName;

		DatabasePlugIn dbPlugIn = DatabasePlugInLoader.getPlugIn();
		String[] columns = {"id"};

		String sql =
				"update EIMOBJSTR set value = replace(value, ? , ?) where value like ? and type = (select id from EIMATTR where name = 'パス') " +
				// returning句
				dbPlugIn.getQueryStringReturning(columns);

		// ステートメント
		Connection conn = DBUtils.getNativeConnection(sess.getDBConnection());
		try (PreparedStatement ps = conn.prepareStatement(sql)) {

			// 入力パラメータ
			ps.setString(1, oldObjName + "/");
			ps.setString(2, newObjName + "/");
			ps.setString(3, oldObjName + "/%");

			// 出力パラメータ
			dbPlugIn.registerReturnParameter(ps, 4, Types.BIGINT);

			// update実行
			ps.execute();

			// 更新されたオブジェクトのIDを取得
			List<Long> updatedIdList = new ArrayList<>();
			try (ResultSet rset = dbPlugIn.getReturnResultSet(ps)) {
				while (rset.next()) {
					updatedIdList.add(rset.getLong(1));
				}
			}

			// 更新ログを登録する
			if (!updatedIdList.isEmpty()) {
				DataCacheDao objectCacheDao = (DataCacheDao) ApplicationContextLoader.getApplicationContext().getBean("objectCacheDao");
				List<ObjectDomain> changeList = updatedIdList.stream().map(id -> new ObjectDomain(id)).collect(Collectors.toList());
				objectCacheDao.putUpdateLogAll(changeList);
			}

		} catch (Exception e) {
			EIMSysException eime = new EIMSysException("EIM.ERROR.SYSTEMERROR");
			eime.initCause(e);
			throw eime;
		}
	}

	/**
	 * 指定オブジェクトを起点にフォルダツリーをたどり、全オブジェクトを処理ウォーカーで処理します。<br>
	 *
	 * @param sess セッション
	 * @param object 起点となるオブジェクト
	 * @param doProcessAllVersions 過去のバージョン全ても取得するどうかのフラグ。trueなら過去バージョンも返却
	 * @param includeDocLink 処理対象にドキュメントリンクを含めるか
	 * @param treeWalker 処理ウォーカー
	 * @param helper 条件判定ヘルパー
	 * @throws Exception
	 */
	public static void processFolderTree(EIMSession sess, EIMObject object,
			boolean doProcessAllVersions, boolean includeDocLink, ProcessFolderTreeWalker treeWalker,
			AppObjectConditionHelper helper) throws Exception {
		if (helper == null)
			helper = new AppObjectConditionHelper(sess);
		if (helper.isTypeOfWorkspaceOrFolder(object.getType())) {
			treeWalker.walkIn(null, object, helper);
		}
		Set alreadyProcessedObjSet = new HashSet();
		processFolderTreeInternal(object, doProcessAllVersions, includeDocLink, treeWalker, helper,
			alreadyProcessedObjSet);
		if (helper.isTypeOfWorkspaceOrFolder(object.getType())) {
			treeWalker.walkOut(object, null, helper);
		}
	}

	/**
	 * 指定オブジェクトを起点にフォルダツリーをたどり、全オブジェクトを処理ウォーカーで処理します。<br>
	 *
	 * @param object 起点となるオブジェクト
	 * @param doProcessAllVersions 過去のバージョン全ても取得するどうかのフラグ。trueなら過去バージョンも返却
	 * @param includeDocLink 処理対象にドキュメントリンクを含めるか
	 * @param treeWalker 処理ウォーカー
	 * @param helper 条件判定ヘルパー
	 * @param alreadyProcessedObjSet 既に処理したオブジェクトのマップ
	 * @throws Exception
	 */
	private static void processFolderTreeInternal(EIMObject object, boolean doProcessAllVersions,
			boolean includeDocLink, ProcessFolderTreeWalker treeWalker, AppObjectConditionHelper helper,
			Set alreadyProcessedObjSet) throws Exception {
		if (object == null || alreadyProcessedObjSet.contains(new Long(object.getId())))
			return;

		if (helper.isTypeOfWorkspaceOrFolder(object.getType())) {
			// フォルダかワークスペース
			// 下位のドキュメントリレーションを取得し、それぞれを再起呼び出し
			List<EIMRelation> rels = helper.getChildRelationListByRelType(object, helper.getRelationTypeOfDocument(), EIMAccessRole.READ);

			for (Iterator<EIMRelation> i = rels.iterator(); i.hasNext();) {
				EIMObject child = i.next().getChild();
				if (helper.isTypeOfFolder(child.getType()))
					treeWalker.walkIn(object, child, helper);

				processFolderTreeInternal(child, doProcessAllVersions, includeDocLink, treeWalker, helper, alreadyProcessedObjSet);

				if (helper.isTypeOfFolder(child.getType()))
					treeWalker.walkOut(child, object, helper);
			}

			// ドキュメントリンク
			if (includeDocLink)
			{
				// リンクリレーションを取得
				List<EIMRelation> relsDocLink = helper.getChildRelationListByRelType(object, helper.getRelationTypeOfDocLink(), EIMAccessRole.READ);
				for (Iterator<EIMRelation> i = relsDocLink.iterator(); i.hasNext();) {
					EIMObject child = i.next().getChild();
					// 子オブジェクトが重複する場合はスキップ
					if (rels.stream().anyMatch(rel -> rel.getChild().getId() == child.getId())) {
						continue;
					}

					// ドキュメントリンクに対するウォーカーによる処理
					treeWalker.processDocument(child, helper);
				}
			}

			return;
		}
		// ドキュメント
		// 引数に応じて過去バージョンを取得して処理する
		if (doProcessAllVersions) {
			EIMVersion version = helper.getVersion(object);
			for (Iterator j = version.getList().iterator(); j.hasNext();) {
				EIMObject pastObj = (EIMObject) j.next();
				if (alreadyProcessedObjSet.contains(new Long(pastObj.getId())))
					continue;
				// ウォーカーによる処理
				treeWalker.processDocument(pastObj, helper);
				alreadyProcessedObjSet.add(new Long(pastObj.getId()));
			}
			// Versionの中に、object自身も含まれるので、ここで終了
			return;
		}
		// ドキュメントに対するウォーカーによる処理
		treeWalker.processDocument(object, helper);
		alreadyProcessedObjSet.add(new Long(object.getId()));
	}

	/**
	 * processFolderTree()が使用する処理ウォーカーを定義します
	 *
	 */
	public interface ProcessFolderTreeWalker {
		/**
		 * 上位フォルダから下位フォルダに再帰移動するタイミングでの処理を行います
		 *
		 * @param upperFolderObjFrom 親フォルダ
		 * @param lowerFolderObjTo 子フォルダ
		 * @param helper 条件判定ヘルパー
		 * @throws Exception
		 */
		void walkIn(EIMObject upperFolderObjFrom, EIMObject lowerFolderObjTo,
				AppObjectConditionHelper helper) throws Exception;

		/**
		 * 下位フォルダから上位フォルダに再帰復帰するタイミングでの処理を行います
		 *
		 * @param lowerFolderObjFrom 子フォルダ
		 * @param upperFolderObjTo 親フォルダ
		 * @param helper
		 * @throws Exception
		 */
		void walkOut(EIMObject lowerFolderObjFrom, EIMObject upperFolderObjTo,
				AppObjectConditionHelper helper) throws Exception;

		/**
		 * 引数ドキュメントオブジェクトに対して処理を行います
		 *
		 * @param docObj 対象ドキュメントオブジェクト
		 * @param helper 判定ヘルパー
		 * @throws Exception
		 */
		void processDocument(EIMObject docObj, AppObjectConditionHelper helper) throws Exception;
	}

	// [09/02/03 added by ik.]
	//  >>>> actPasteObjectLink.jsp からドキュメントリンク作成部分を移動し、微修正
	// [09/03/18 modified by ik.]
	//  >>>> ドキュメントリンク作成先フォルダのパス直接指定のパターン(ドキュメントリンク含むフォルダツリー複製用)を作成し、
	//  >>>> 共通処理（createDocLinkCommonProc()）を外出し。
	/**
	 * ドキュメントオブジェクトのドキュメントリンクを作成します。
	 * アクセス履歴/操作履歴を作成するかどうかを切り替えることもできます。
	 *
	 * @param srcObj ドキュメントオブジェクト（ドキュメントリンク作成元）
	 * @param dstFolderObj ドキュメントリンク作成先のフォルダオブジェクト
	 * @param isCreateAccess アクセス履歴を作成するかどうか
	 * @param isCreateOpeHist 操作履歴を作成するかどうか
	 * @param helper 条件判定ヘルパー
	 * @param linkUpdateTiming リンク更新タイミング
	 * @throws Exception
	 */
	public static void createDocLink(EIMObject srcObj, EIMObject dstFolderObj,
			boolean isCreateAccess, boolean isCreateOpeHist, AppObjectConditionHelper helper, long linkUpdateTiming) throws Exception {

		//リンク貼り付け先Objectが取得できない場合
		if(dstFolderObj == null) {
			throw new EIMException(helper.getSession(), "EIM.ERROR.LOGIC.NOFOLDERORDOCUMENT");
		}

		//リンク貼り付け先がゴミ箱の場合
		if (AppObjectUtil.isObjectInRecycle(helper.getSession(), dstFolderObj)) {
			throw new EIMException(helper.getSession(), "EIM.ERROR.INPUT.PASTETORECYCLE");
		}

		// 貼付け先オブジェクトのセキュリティのチェック
		EIMSecurity dstFolderObjSec = dstFolderObj.getSecurity();
		if (dstFolderObjSec != null) {
			if (!SecurityUtils.authorized(helper.getSession(),dstFolderObj, helper.getSession().getUser(), EIMAccessRole.CREATE)) {
				// リンク貼付け権限がありません。
				throw new EIMException(helper.getSession(), "EIM.ERROR.LOGIC.NODOCLINKPASTEROLE");
			}
		}

		AppLogicUtil.createDocLinkCommonProc(srcObj, dstFolderObj, null, isCreateAccess, isCreateOpeHist, helper,
				linkUpdateTiming, null, null);
	}

	/**
	 * フォルダツリー複製用
	 * @param srcObj ドキュメントオブジェクト（ドキュメントリンク作成元）
	 * @param dstFolderObj ドキュメントリンク作成先のフォルダオブジェクト
	 * @param dstFolderPath ドキュメントリンク作成先フォルダのパス
	 * @param isCreateAccess アクセス履歴を作成するかどうか
	 * @param isCreateOpeHist 操作履歴を作成するかどうか
	 * @param helper ヘルパークラス
	 * @param linkUpdateTiming リンク更新タイミング
	 * @param childObjVersionMap 子オブジェクトIDをキーにして、バージョンIDを値に持つ。
	 * @param parentObjVersionListMap 親オブジェクトIDをキーにして、処理したリビジョン番号のリストを値に持つ。
	 * @throws Exception
	 */
	public static void createDocLink(EIMObject srcObj, EIMObject dstFolderObj,
			String dstFolderPath, boolean isCreateAccess, boolean isCreateOpeHist, AppObjectConditionHelper helper,
			int linkUpdateTiming, HashMap<Long, Long> childObjVersionMap, HashMap<Long, List> parentObjVersionListMap) throws Exception {

		AppLogicUtil.createDocLinkCommonProc(srcObj, dstFolderObj, dstFolderPath, isCreateAccess, isCreateOpeHist, helper,
				linkUpdateTiming, childObjVersionMap, parentObjVersionListMap);

	}

	// 内部メソッド
	private static void createDocLinkCommonProc(EIMObject srcObj, EIMObject dstFolderObj, String dstFolderPath,
			boolean isCreateAccess, boolean isCreateOpeHist, AppObjectConditionHelper helper, long linkUpdateTiming,
			HashMap<Long, Long> childObjVersionMap, HashMap<Long, List> parentObjVersionListMap) throws Exception {

		//リンク作成対象Objectが取得できない場合
		if(srcObj == null) {
			throw new EIMException(helper.getSession(), "EIM.ERROR.LOGIC.NOTEXIST.DOCFOL");
		}

		//リンク作成対象Objectがフォルダの場合
		if (helper.isTypeOfFolder(srcObj.getType())) {
			throw new EIMException(helper.getSession(), "EIM.ERROR.LOGIC.CANTLINKPASETFOLDER");
		}

		// コピー元オブジェクトのセキュリティのチェック
		EIMSecurity srcObjSec = srcObj.getSecurity();
		if (srcObjSec != null) {
			if (!helper.authorized(srcObj, helper.getSession().getUser(), EIMAccessRole.READ)) {
				// リンク貼付け権限がありません。
				throw new EIMException(helper.getSession(), "EIM.ERROR.LOGIC.NODOCLINKPASTEROLE");
			}
		}

		// コピー元がドキュメントの場合
		if (helper.isTypeOfDocument(srcObj.getType())) {
			// コピー元オブジェクトのステータスチェック
			long stsDocKind = srcObj.getStatus() != null ? srcObj.getStatus().getType().getKind() : AppConstant.STATUS_TYPE_KIND_ID_NONE;
			// ステータスが「なし」「公開中」以外なら権限なし。
			if( (stsDocKind != AppConstant.STATUS_TYPE_KIND_ID_NONE) && (stsDocKind != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC) ) {
				// リンク貼付け権限がありません。
				throw new EIMException(helper.getSession(), "EIM.ERROR.LOGIC.NODOCLINKPASTEROLE");
			}
		}

		// コピー先のステータス種別ID
		long stsKind = dstFolderObj.getStatus() != null ? dstFolderObj.getStatus().getType().getKind() : AppConstant.STATUS_TYPE_KIND_ID_NONE;

		// 上位WFフォルダのステータスのチェック
		if ((helper.isTypeOfFolderWithWorkflow(dstFolderObj)
				|| helper.isTypeOfFolderUnderFolderWithWorkflow(dstFolderObj))
					&& stsKind != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {	// 上位WFのステータスが「編集中」以外の場合はエラー
			// コピー/貼付け権限がありません。
			throw new EIMException(helper.getSession(), "EIM.ERROR.LOGIC.NODOCLINKPASTEROLE");
		}

		// リンク更新タイミングが公開時更新の場合、同一リビジョンのリンク重複チェック
		// 手動はチェック不要
		String opeHistType;
		if( linkUpdateTiming == AppConstant.LINK_UPDATE_TIMING_AUTO ) {
			boolean flag = isDuplicateLinkRelationAuto(helper, dstFolderObj, srcObj, childObjVersionMap, parentObjVersionListMap);
			if( flag ) {
				throw new EIMException(helper.getSession(), "EIM.ERROR.LOGIC.DOCLINK.AUTOLINK.EXIST");
			}
			opeHistType = AppConstant.DOCLINK_CREATE_AUTO;
		}
		else {
			// 手動更新
			opeHistType = AppConstant.DOCLINK_CREATE;
		}

		// [09/03/18 modified by ik.]
		// ドキュメントリンク作成先フォルダパスが直接指定されている場合(ドキュメントリンク含むフォルダツリー複製時)は
		// それを使う。
		String path = dstFolderPath;
		if (path == null) {
			path = AppObjectUtil.getPath(dstFolderObj);
			if(path == null){
				// ワークスペースの場合、パス属性の値を保持していない
				path = "/";
			}
			path += dstFolderObj.getName() + "/";
		}

		// 「パス」「リンク先」属性に、ドキュメントリンクの親オブジェクト情報を追加
		AppLogicUtil.setDocLinkPath(helper.getSession(), srcObj,dstFolderObj.getId(), path, helper);

		// 「リンク」リレーションの作成
		EIMRelation relation = RelationUtils.createRelation(helper.getSession(), helper.getRelationTypeOfDocLink(), dstFolderObj, srcObj);

		// 「リンク」リレーションの属性設定
		AppLogicUtil.setAttributeLinkRelation(helper.getSession(), relation, linkUpdateTiming);

		//Access
		if (isCreateAccess) {
			AccessUtils.createAccess(helper.getSession(), srcObj, "EIM.ACCESS.TYPE.DOCLINK.CREATE");
		}

		//Create Operation History
		if (isCreateOpeHist) {
			OperationHistoryUtils.create(helper.getSession(), AppConstant.DOCUMENT, opeHistType,
					AppConstant.LINK_ORIGIN, EIMConstant.OBJECT, srcObj,
					EIMConstant.TARGET_CREATE, EIMConstant.OBJECT, dstFolderObj, path);
		}
	}

	/**
	 * オブジェクトの「パス」「リンク先」属性に、ドキュメントリンクの親オブジェクト情報を追加します
	 *
	 * @param sess EIMSessionインスタンス
	 * @param object リンク元オブジェクト
	 * @param parentObjId リンク先(親)オブジェクトのObjctId
	 * @param path リンク先(親)オブジェクトのパス
	 * @throws Exception
	 */
	public static void setDocLinkPath(EIMSession sess, EIMObject object, long parentObjId, String path,
			AppObjectConditionHelper helper) throws Exception {

		// リンク元オブジェクトの「パス」属性(複数値型)を取得
		String[] pathArray = helper.getStrAttrsPath(object);
		String[] newPathArray = null;
		if( pathArray != null ) {
			// リンク先のパスを追加して、オブジェクトに再設定
			newPathArray = new String[pathArray.length + 1];
			System.arraycopy(pathArray, 0, newPathArray, 0, pathArray.length);
			newPathArray[pathArray.length] = path;
		}
		else {	// 属性の新規作成(パス属性が無い時点でエラーと思われるが)
			newPathArray = new String[1];
			newPathArray[0] = path;
		}

		//「パス」属性を更新する
		helper.setAttrPath(object, newPathArray);

		// リンク元オブジェクトの「リンク先」属性(複数値型)を取得
		long[] linkArray = helper.getIntAttrsToLink(object);
		long[] newLinkArray = null;
		if( linkArray != null ) {
			// リンク先のObjectIdを追加して、オブジェクトに再設定
			newLinkArray = new long[linkArray.length + 1];
			System.arraycopy(linkArray, 0, newLinkArray, 0, linkArray.length);
			newLinkArray[linkArray.length] = parentObjId;
		}
		else {	// 属性の新規作成
			newLinkArray = new long[1];
			newLinkArray[0] = parentObjId;
		}
		//「リンク先」属性を更新する
		helper.setAttrToLink(object, newLinkArray);

		// リンク元オブジェクトの「ドキュメントリンク」属性に1を設定
		ObjectAttributeUtils.setAttribute(sess, object, helper.getAttrType(EIMConfig.get("ATTR_NAME_DOCUMENT_DOCUMENT_LINK")), 1);
	}

	/**
	 * オブジェクトの「パス」「リンク先」属性に対して、以下を行います
	 *
	 * <li>切り取り元オブジェクト情報を削除
	 * <li>貼り付け先オブジェクト情報を追加
	 *
	 * @param sess EIMSessionインスタンス
	 * @param object リンク元オブジェクト
	 * @param fromObjId リンク切り取り元(親)オブジェクトのObjctId
	 * @param toObjId リンク貼り付け先(親)オブジェクトのObjctId
	 * @param path リンク貼り付け先(親)オブジェクトのパス
	 * @return >=0:正常終了、-1:異常終了、-2:「リンク先」属性にリンク切り取り元(親)オブジェクトのObjctIdが存在しない
	 * @throws Exception
	 */
	public static int replaceDocLinkPath(EIMSession sess, EIMObject object, long fromObjId, long toObjId, String path) throws Exception {

		// リンク元オブジェクトの「リンク先」属性(複数値型)を取得
		long[] linkArray = AppObjectUtil.getIntAttrs(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_TARGET_TO_LINK"));
		// リンク元オブジェクトの「パス」属性(複数値型)を取得
		String[] pathArray = AppObjectUtil.getStrAttrs(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_PASS"));

		boolean bFound = false;
		if( ( linkArray != null ) && ( pathArray != null ) && ( (linkArray.length + 1) == pathArray.length ) ) {	// この条件に合致しない場合は不具合
			for( int i = 0; i < linkArray.length; i++ ) {
				if( linkArray[i] == fromObjId ) {	// 切り取り元ObjIdを貼り付け先ObjIdに置換する
					linkArray[i] = toObjId;
					// 同時にパスも置換する
					pathArray[i+1] = path;
					bFound = true;
				}
			}
		}
		else {
			return REPLACE_DOCLINK_PATH_ERROR;
		}

		// 切り取り元ObjIdが見つからなかった場合
		if( !bFound )
			return REPLACE_DOCLINK_PATH_ERROR_DOCLINK_MOVED;

		//「パス」属性を更新する時に、権限認証を外す、理由はチケット415を参照してください
		// 「リンク先」属性の再設定
		//AppObjectUtil.setAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_TARGET_TO_LINK"), linkArray);
		ObjectAttributeUtils.setAttribute(sess, object, AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_TARGET_TO_LINK"))
				, TypeConvertUtils.convertToBuildTypeArray(linkArray));
		// 「パス」属性の再設定
		//AppObjectUtil.setAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_PASS"), pathArray);
		ObjectAttributeUtils.setAttribute(sess, object, AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_PASS")), pathArray);
		// リンク元オブジェクトの「ドキュメントリンク」属性に1を設定 (既に設定されているはずだが念のため)
		//AppObjectUtil.setAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_DOCUMENT_LINK"), 1);
		ObjectAttributeUtils.setAttribute(sess, object, AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_DOCUMENT_LINK")), 1);

		return 0;	// 正常終了
	}

	/**
	 * オブジェクトの「パス」「リンク先」属性に対して、以下を行います
	 *
	 * <li>削除オブジェクト情報を削除
	 *
	 * @param sess EIMSessionインスタンス
	 * @param object リンク元オブジェクト
	 * @param delObjId 削除対象オブジェクトのObjctId
	 * @throws Exception
	 */
	public static void deleteDocLinkPath(EIMSession sess, EIMObject object, long delObjId) throws Exception {

		// リンク元オブジェクトの「リンク先」属性(複数値型)を取得
		long[] linkArray = AppObjectUtil.getIntAttrs(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_TARGET_TO_LINK"));
		// リンク元オブジェクトの「パス」属性(複数値型)を取得
		String[] pathArray = AppObjectUtil.getStrAttrs(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_PASS"));

		if( ( linkArray != null ) && ( pathArray != null ) && ( (linkArray.length + 1) == pathArray.length ) ) {	// この条件に合致しない場合は不具合と思われる

			long[] linkTmp = new long[linkArray.length];
			String[] pathTmp = new String[pathArray.length];
			pathTmp[0] = pathArray[0];	// 最初の「パス」はリンク元ドキュメントのパス

			int len = 0;
			for( int i = 0; i < linkArray.length; i++ ) {
				if( linkArray[i] != delObjId ) {
					linkTmp[len] = linkArray[i];
					pathTmp[len + 1] = pathArray[i + 1];
					len++;
				}
			}
			if( len == 0 ) { //「リンク先」属性が0個になった場合
				// 「リンク先」属性の削除
				AppObjectUtil.deleteAttribute(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_TARGET_TO_LINK"));
				// 「ドキュメントリンク」属性を0に設定
				//AppObjectUtil.setAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_DOCUMENT_LINK"), 0);
				ObjectAttributeUtils.setAttribute(sess, object, AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_DOCUMENT_LINK")), 0);
			}
			else {
				// 「リンク先」属性に削除後の値を再設定
				long[] linkSet = new long[len];
				System.arraycopy(linkTmp, 0, linkSet, 0, len);
				//AppObjectUtil.setAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_TARGET_TO_LINK"), linkSet);
				ObjectAttributeUtils.setAttribute(sess, object, AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_TARGET_TO_LINK"))
						, TypeConvertUtils.convertToBuildTypeArray(linkSet));
			}
			// 「パス」属性に削除後の値を再設定
			String[] pathSet = new String[len + 1];
			System.arraycopy(pathTmp, 0, pathSet, 0, len + 1);
			//AppObjectUtil.setAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_PASS"), pathSet);
			ObjectAttributeUtils.setAttribute(sess, object, AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_PASS")), pathSet);

		}
	}

	/**
	 * 指定フォルダ直下のドキュメントリンクについて、リンク元ドキュメントのパス属性を変更します
	 *
	 * <li>本関数ではフォルダの再帰取得は行わない(呼び出し元で行うようにしてください)
	 *
	 * @param sess EIMSessionインスタンス
	 * @param object フォルダのオブジェクト
	 * @param path フォルダのパス
	 * @param conditionHelper ヘルパーインスタンス
	 * @throws Exception
	 */
	public static void renameDocLinkPathUnderFolder(EIMSession sess, EIMObject object, String path, AppObjectConditionHelper helper) throws Exception {

		//Relation Type
		EIMRelationType relType = helper.getRelationTypeOfDocLink();

		// 子リレーション一覧取得
		List<EIMRelation> childRelList = helper.getChildRelationListByRelType(object, relType, EIMAccessRole.NONE);
		for (int i = 0; i < childRelList.size(); i++) {
			EIMRelation rel = (EIMRelation) childRelList.get(i);
			EIMObject obj = ( rel != null ) ? rel.getChild() : null;
			if( obj != null ) {
				// リンク元オブジェクトの「リンク先」属性(複数値型)を取得
				long[] linkArray = AppObjectUtil.getIntAttrs(sess, obj, EIMConfig.get("ATTR_NAME_DOCUMENT_TARGET_TO_LINK"));
				// リンク元オブジェクトの「パス」属性(複数値型)を取得
				String[] pathArray = helper.getStrAttrsPath(obj);

				boolean bFound = false;
				if( ( linkArray != null ) && ( pathArray != null ) && ( (linkArray.length + 1) == pathArray.length ) ) {	// この条件に合致しない場合は不具合
					for( int j = 0; j < linkArray.length; j++ ) {
						if( linkArray[j] == object.getId() ) {
							// パスを置換する
							pathArray[j+1] = path;
							bFound = true;
						}
					}
				}
				else {
					continue;
				}
				// パスの変換を行った場合
				if( bFound ) {
					// 「パス」属性の再設定
					helper.setAttrPath(obj, pathArray);
				}
			}
		}
	}

	/**
	 * フォルダ配下のドキュメントのバージョン情報を再帰的に取得します
	 *
	 * @param sess EIMSessionインスタンス
	 * @param folderObj フォルダオブジェクト
	 * @param relTypeDoc ドキュメントリレーションタイプ
	 * @param helper ドキュメント管理オブジェクト判定の支援クラスのインスタンス
	 * @param sessHelper EIMSessionの付け替え支援クラス
	 * @param versionList ヴァージョン情報のリストの設定先
	 * @throws Exception
	 */
	public static void getDocumentVersionReccurently(EIMSession sess, EIMObject folderObj, EIMRelationType relTypeDoc,
			AppObjectConditionHelper helper, List versionList) throws Exception {

		// フォルダ配下のリレーションリストを取得
		List childRelList = helper.getChildRelationListByRelType(folderObj, relTypeDoc, EIMAccessRole.NONE);

		for(int i = 0; i < childRelList.size(); i++){

			//Relation
			EIMRelation relation = (EIMRelation)childRelList.get(i);

			//Child Object
			EIMObject childObj = relation.getChild();

			// オブジェクトタイプがフォルダなら再起呼び出し
			if( helper.isTypeOfFolder( childObj.getType()) ) {
				getDocumentVersionReccurently(sess, childObj, relTypeDoc, helper, versionList);
			} else {
				EIMVersion version = helper.getVersion(childObj);
				// リストに追加
				versionList.add(version);
			}
		}
	}

	/**
	 * フォルダ配下のドキュメントのブランチ情報を論理削除します。
	 * フォルダ配下にブランチの親となるドキュメントがある場合、例外を投げます。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param folderObj フォルダオブジェクト
	 * @param relTypeBranch ブランチリレーションタイプ
	 * @param relTypeDoc ドキュメントリレーションタイプ
	 * @param helper ドキュメント管理オブジェクト判定の支援クラスのオブジェクト
	 * @param sessHelper EIMSessionの付け替え支援クラス
	 * @throws Exception
	 */
	public static void logicalDeleteFolderBranch(EIMSession sess, EIMObject folderObj, EIMRelationType relTypeBranch,
			EIMRelationType relTypeDoc, AppObjectConditionHelper helper) throws Exception {

		// フォルダ配下のドキュメントバージョンを再帰的に取得
		List<EIMVersion> versionList = new ArrayList<>();
		getDocumentVersionReccurently(sess, folderObj, relTypeDoc, helper, versionList);

		// バージョンリストの中にブランチの親となっている
		for(int i = 0 ; i < versionList.size() ; i++){
			EIMVersion version = (EIMVersion)versionList.get(i);
			if(checkVersionHasBranchParent(sess, version, relTypeBranch, helper)){
				// ある場合は例外をスロー
				throw new EIMException(sess, "EIM.ERROR.LOGIC.CNATDEL.BRUNCHDOC.EXIST");
			}
		}

		// 子になっているブランチリレーションの削除済みフラグを設定する
		for(int j = 0 ; j < versionList.size() ; j++){
			setDeleteFlagOn(sess, (EIMVersion)versionList.get(j), relTypeBranch, helper);
		}
	}

	/**
	 * ドキュメントのブランチ情報を論理削除します。
	 * 当該ドキュメントを親とするブランチがある場合は例外を投げます。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param object 対象のドキュメントオブジェクト
	 * @param relTypeBranch ブランチリレーションタイプ
	 * @param sessHelper EIMSessionの付け替え支援クラス
	 * @throws Exception
	 */
	public static void logicalDeleteDocumentBranch(EIMSession sess, EIMObject object, EIMRelationType relTypeBranch) throws Exception {
		logicalDeleteDocumentBranch(sess, object, relTypeBranch, null);
	}

	/**
	 * ドキュメントのブランチ情報を論理削除します。
	 * 当該ドキュメントを親とするブランチがある場合は例外を投げます。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param object 対象のドキュメントオブジェクト
	 * @param relTypeBranch ブランチリレーションタイプ
	 * @param sessHelper EIMSessionの付け替え支援クラス
	 * @param conditionHelper 条件判定ヘルパーインスタンス
	 * @throws Exception
	 */
	public static void logicalDeleteDocumentBranch(EIMSession sess, EIMObject object, EIMRelationType relTypeBranch, AppObjectConditionHelper conditionHelper) throws Exception {

		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = conditionHelper;
		if (helper == null) {
			helper = new AppObjectConditionHelper(sess);
		}

		// 削除対象オブジェクトを全バージョン取得
		EIMVersion version = VersionUtils.getVersion(sess, object);

		// 取得したバージョンの中に親ブランチとなっているドキュメントがないか
		if(checkVersionHasBranchParent(sess, version, relTypeBranch, helper)){
			// ある場合は例外をスロー
			throw new EIMException(sess, "EIM.ERROR.LOGIC.CNATDEL.BRUNCHDOC.EXIST");
		}

		// 削除フラグの設定
		setDeleteFlagOn(sess, version, relTypeBranch, helper);
	}

	/**
	 * 対象のバージョン情報の中にブランチの親になっているオブジェクトがあるか否かを返します
	 *
	 * @param sess EIMSessionインスタンス
	 * @param version 対象のEIMVersion情報
	 * @param relTypeBranch ブランチリレーションタイプ
	 * @param sessHelper EIMSessionの付け替え支援クラスのインスタンス
	 * @return 対象のバージョン情報の中にブランチの親になっているオブジェクトがあるか否か
	 * @throws Exception
	 */
	private static boolean checkVersionHasBranchParent(EIMSession sess, EIMVersion version,
			EIMRelationType relTypeBranch, AppObjectConditionHelper helper) throws Exception {

		// バージョンごとにループ
		@SuppressWarnings("unchecked")
		List<EIMObject> versionList = version.getList();
		for(int i = 0 ; i < versionList.size(); i++){

			// 各バージョンのオブジェクトを親とするブランチリレーションを取得
			EIMObject targetObj = versionList.get(i);
			List<EIMRelation> branchRelList = helper.getChildRelationListByRelType(targetObj, relTypeBranch, EIMAccessRole.NONE);
			for(int j = 0 ; j < branchRelList.size(); j++){
				// ブランチリレーションを取得する
				EIMRelation branchRel = (EIMRelation)branchRelList.get(j);
				EIMAttribute attrBranchTargetDelete = branchRel.getAttribute(EIMConfig.get("ATTR_NAME_BRANCH_TARGET_DELETE"));

				// ブランチ先が削除済みでないオブジェクトが存在する
				if(attrBranchTargetDelete == null ||
				   attrBranchTargetDelete.getInts()[0] == AppConstant.FLAG_OFF ){
					// ブランチの親になっているオブジェクトあり
					return true;
				}
			}
		}
		// バージョンの中に、親ブランチが存在しなかった
		return false;
	}

	/**
	 * 対象のバージョン情報を子とするブランチリレーションのブランチ先削除済みフラグをONに設定します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param version 対象のEIMVersion情報
	 * @param relTypeBranch ブランチリレーションタイプ
	 * @param sessHelper EIMSessionの付け替え支援クラスのインスタンス
	 * @throws Exception
	 */
	private static void setDeleteFlagOn(EIMSession sess, EIMVersion version, EIMRelationType relTypeBranch, AppObjectConditionHelper helper) throws Exception {

		// 一番古いオブジェクトの取得
		EIMObject object = (EIMObject)version.getList().get(0);
		setDeleteFlagOn(sess, object, relTypeBranch, helper);

	}

	/**
	 * 対象のバージョン情報を子とするブランチリレーションのブランチ先削除済みフラグをONに設定します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param object 対象のEIMObject
	 * @param relTypeBranch ブランチリレーションタイプ
	 * @param sessHelper EIMSessionの付け替え支援クラスのインスタンス
	 * @throws Exception
	 */
	private static void setDeleteFlagOn(EIMSession sess, EIMObject object, EIMRelationType relTypeBranch, AppObjectConditionHelper helper) throws Exception
	{

		// ブランチリレーションの取得
		List<EIMRelation> parentBranchRelList = helper.getParentRelationListByRelType(object, relTypeBranch, EIMAccessRole.NONE);
		if(parentBranchRelList.size() >= 1) {
			// ブランチ先削除属性に論理削除済みを設定
			EIMRelation parentBranchRel = parentBranchRelList.get(0);
			EIMAttributeType attrTypeBranchTargetDelete = helper.getAttrType(EIMConfig.get("ATTR_NAME_BRANCH_TARGET_DELETE"));

			RelationAttributeUtils.setAttribute(sess, parentBranchRel, attrTypeBranchTargetDelete, AppConstant.FLAG_ON);

			// SearchFramework  更新通知 対象：ブランチ元ドキュメント
			AppUpdateNoticeUtils.updateNoticeInsert(parentBranchRel.getParent().getId(), updateNoticeKeyBrachDoc);
		}
	}

	/**
	 * フォルダ配下のドキュメントのブランチ情報を論理削除から復帰します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param folderObj フォルダオブジェクト
	 * @param relTypeBranch ブランチリレーションタイプ
	 * @param relTypeDoc ドキュメントリレーションタイプ
	 * @param helper ドキュメント管理オブジェクト判定の支援クラスのオブジェクト
	 * @param sessHelper EIMSessionの付け替え支援クラス
	 * @throws Exception
	 */
	public static void returnFolderBranch(EIMSession sess, EIMObject folderObj, EIMRelationType relTypeBranch, EIMRelationType relTypeDoc, AppObjectConditionHelper helper) throws Exception {

		// フォルダ配下のリレーションリストを取得
		List childRelList = helper.getChildRelationListByRelType(folderObj, relTypeDoc, EIMAccessRole.READ);

		for(int i = 0; i < childRelList.size(); i++){

			//Relation
			EIMRelation relation = (EIMRelation)childRelList.get(i);

			//Child Object
			EIMObject childObj = relation.getChild();

			// オブジェクトタイプがフォルダなら再起呼び出し
			if( helper.isTypeOfFolder( childObj.getType()) ) {
				returnFolderBranch(sess, childObj, relTypeBranch, relTypeDoc, helper);
			}
			// ドキュメントの場合、ブランチを論理削除
			else{
				returnDocumentBranch(sess, childObj, relTypeBranch, helper);
			}

		}
	}

	/**
	 * ドキュメントのブランチ情報を論理削除から復帰します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param object 対象のドキュメントオブジェクト
	 * @param relTypeBranch ブランチリレーションタイプ
	 * @param sessHelper EIMSessionの付け替え支援クラス
	 * @throws Exception
	 */
	public static void returnDocumentBranch(EIMSession sess, EIMObject object, EIMRelationType relTypeBranch, AppObjectConditionHelper helper) throws Exception {

		// 一番古いバージョンのオブジェクトを取得
		EIMObject rootObj = (EIMObject) helper.getVersion(object).getList().get(0);

		// 一番古いバージョンのオブジェクトを子とするブランチリレーションを取得
		List<EIMRelation> relList = helper.getParentRelationListByRelType(rootObj, relTypeBranch, EIMAccessRole.NONE);

		if(relList.size() >= 1) {

			// ブランチ先削除属性に「論理削除していない」を設定
			EIMRelation branchRel = relList.get(0);
			EIMAttributeType attrTypeBranchTargetDelete = helper.getAttrType(EIMConfig.get("ATTR_NAME_BRANCH_TARGET_DELETE"));

			RelationAttributeUtils.setAttribute(sess, branchRel, attrTypeBranchTargetDelete, AppConstant.FLAG_OFF);

			// ### SEARCH FRAMEWORK 検索FW更新通知 ブランチ元ドキュメント、処理種別キーを指定(切り取り元がゴミ箱のみ)
			AppUpdateNoticeUtils.updateNoticeInsert(branchRel.getParent().getId(), "SEARCHFW_CUTPASTE_ORG_DOCUMENT");
		}

		// ### SEARCH FRAMEWORK 検索FW更新通知 ドキュメント復帰、処理種別キーを指定(切り取り元がゴミ箱のみ)
		AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_CUTPASTE_FROM_RECYCLE_BOX");
	}

	/**
	 * タグを物理削除します。
	 *
	 * ※ タグが有効なものであった場合(有効期限が設定されており、かつ有効期限内)例外を返却します
	 *
	 * @param sess EIMSessionインスタンス
	 * @param object 対象のタグオブジェクト
	 * @param parentObj タグの親オブジェクト
	 * @param relTypeDoc ドキュメントリレーションのタイプ
	 * @param sessHelper EIMSessionの付け替え支援クラス
	 * @throws Exception
	 */
	public static void physicalDeleteTag(EIMSession sess, EIMObject object, EIMObject parentObj, EIMRelationType relTypeDoc) throws Exception {

		// 有効期限のチェック
		Date effectDate = AppObjectUtil.getDateAttr(sess, object, EIMConfig.get("ATTR_NAME_FOLDER_EFFECT_DATE"));	// 有効期限
		if (effectDate != null) {
			// システム日付 ＜＝ 対象オブジェクトの属性値「有効期限」の場合はエラー
			if (!DateUtils.judgeExpirationDate(sess, effectDate)) {
				// 有効なドキュメント、またはフォルダは削除できません。
				throw new EIMException(sess, "EIM.ERROR.LOGIC.NOTDEL.EFFECTIVE.DOCFOL");
			}
		}

		List objList = TagUtil.getTagGivenObj(sess, object);
		if (objList != null) {
			EIMThreadContext.put("SEARCHFW.TAG.PATTERN", "physicalDeleteTag");
			for (Iterator i = objList.iterator(); i.hasNext();) {
				EIMObject obj = (EIMObject)i.next();
				// タグの解除
				TagUtil.removeTag(sess, obj, object, false);
			}
		}

		// ドキュメントリレーションの削除
		EIMRelation rel = RelationUtils.getRelationByParentAndChild(sess, relTypeDoc, parentObj, object);
		if(!SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.DELETE_RELATION)){
			throw new EIMException(sess, "EIM.ERROR.LOGIC.NODELETERELROLE");
		}
		RelationUtils.deleteRelation(sess, rel);

		// 属性表示色オブジェクト削除
		DisplayColorUtil.deleteDisplayColorObject(sess, object);
		if(!SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.DELETE)){
			throw new EIMException(sess, "EIM.ERROR.LOGIC.NODELETEROLE");
		}
		// タグオブジェクトの削除
		ObjectUtils.deleteObject(sess, object);
	}

	/**
	 * フォルダ配下のタグを物理削除します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param object 対象のフォルダオブジェクト
	 * @param relTypeDoc ドキュメントリレーションのタイプ
	 * @param sessHelper EIMSessionの付け替え支援クラス
	 * @param condHelper ドキュメント管理オブジェクト判定の支援クラス
	 * @throws Exception
	 */
	public static void physicalDeleteTagUnderFolder(EIMSession sess, EIMObject object, EIMRelationType relTypeDoc, AppObjectConditionHelper condHelper) throws Exception {

		// 配下オブジェクトの取得
		List childList = condHelper.getChildObjectsWithoutAnyConditions(object, false);

		for (Iterator i = childList.iterator(); i.hasNext();) {
			EIMObject tmpObj = (EIMObject) i.next();
			// タグであれば
			if(condHelper.isTypeOfTag(tmpObj.getType())) {
				// タグの物理削除
				physicalDeleteTag(sess, tmpObj, object, relTypeDoc);

				// SearchFramework 削除通知 対象：フォルダ配下のタグ(外部IF、通常の物理削除にて呼び出し)
				AppUpdateNoticeUtils.updateNoticeDelete(tmpObj.getId(), "SEARCHFW_PHYSICALDEL_TAG");
			}
			// フォルダであれば
			else if(condHelper.isTypeOfFolder(tmpObj.getType())) {
				// 再帰処理
				physicalDeleteTagUnderFolder(sess, tmpObj, relTypeDoc, condHelper);

			}
		}
	}

	/**
	 * 任意のタグが付与されたドキュメント・フォルダ・タグについて、別のタグの付与を行います。
	 * (タグのコピー時に使用)
	 *
	 * @param sess EIMSessionインスタンス
	 * @param srcTag コピー元のタグオブジェクト
	 * @param destTag コピー先のタグオブジェクト
	 * @param user 操作を実施するユーザー
	 * @param helper 条件判定ヘルパー
	 * @param sessHelper EIMSessionの付け替え支援クラス
	 * @throws Exception
	 */
	public static void copyAssignedTag(EIMSession sess, EIMObject srcTag, EIMObject destTag, EIMUser user, AppObjectConditionHelper helper) throws Exception {

		// タグが付与されているオブジェクトを収集
		List objList = null;
		if( (objList = TagUtil.getTagGivenObj(sess, srcTag)) != null) {

			// 属性タイプの冗長な取得回避用Mapを生成
			HashMap attrTypeMap = TagUtil.getTagAttrTypeMap(sess);

			EIMThreadContext.put("SEARCHFW.TAG.PATTERN", "None");
			for (Iterator i = objList.iterator(); i.hasNext();) {
				EIMObject obj = (EIMObject)i.next();
				// タグの付与(再帰処理は行わない)
				TagUtil.assignTag(sess, obj, destTag, user, attrTypeMap, helper, false);
			}
		}
	}

	/**
	 * オブジェクト(ドキュメント・フォルダ・タグ)、あるいはその配下にタグが付与されているかどうかを判定する。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param object 対象のフォルダオブジェクト
	 * @param sessHelper EIMSessionの付け替え支援クラス
	 * @param condHelper ドキュメント管理オブジェクト判定の支援クラス
	 * @param isRecursive フォルダ配下のドキュメント・フォルダ・タグについて再帰処理を行うかどうか
	 * @return オブジェクトあるいはその配下にタグが付与されているかどうか
	 * @throws Exception
	 */
	public static boolean isTagAssignedObject(EIMSession sess, EIMObject object, AppObjectConditionHelper condHelper, boolean isRecursive) throws Exception {

		// タグの場合
		if(condHelper.isTypeOfTag(object.getType())) {
			return isTagAssigned(sess, object);
		}
		// ドキュメントの場合
		else if(condHelper.isTypeOfDocument(object.getType())) {
			return isTagAssignedDocument(sess, object, condHelper);
		}
		// フォルダの場合
		else if(condHelper.isTypeOfFolder(object.getType())) {
			if( isTagAssigned(sess, object) ) {
				return true;
			}
			// 再帰処理
			if(isRecursive) {
				// 配下オブジェクトの取得
				List childList = condHelper.getChildObjectsWithoutAnyConditions(object, false);

				for (Iterator i = childList.iterator(); i.hasNext();) {
					EIMObject tmpObj = (EIMObject) i.next();

					if( isTagAssignedObject(sess, tmpObj, condHelper, true) ) {
						return true;
					}
				}
			}
		}
		return false;
	}

	/**
	 * ドキュメントにタグが付与されているかどうかを判定する。
	 * ※ 判定はドキュメントの全履歴について行う
	 *
	 * @param sess EIMSessionインスタンス
	 * @param object 対象のドキュメントオブジェクト
	 * @return trueならタグが付与されている
	 * @throws Exception
	 */
	public static boolean isTagAssignedDocument(EIMSession sess, EIMObject object, AppObjectConditionHelper helper) throws Exception
	{

		// 対象オブジェクトの全バージョン取得
		EIMVersion version = helper.getVersion(object);

		@SuppressWarnings("unchecked")
		List<EIMObject> objList = version.getList();
		for(int ii = 0; ii < objList.size(); ii++) {
			// タグの判定
			if( isTagAssigned(sess, (EIMObject)objList.get(ii)) ) {
				return true;
			}
		}
		return false;
	}

	/**
	 * オブジェクト(ドキュメント・フォルダ・タグ)にタグが付与されているかどうかを判定する
	 * @param sess EIMSessionインスタンス
	 * @param object 対象のオブジェクト
	 * @return trueならタグが付与されている
	 * @throws Exception
	 */
	private static boolean isTagAssigned(EIMSession sess, EIMObject object) throws Exception
	{
		ArrayList tmpAttrList = (ArrayList)object.getAttributeList();
		String nameTag = EIMConfig.get("ATTR_NAME_FOLDER_TAG");
		for(int jj = 0; jj < tmpAttrList.size(); jj++) {
			if(((EIMAttribute)tmpAttrList.get(jj)).getType().getDefName().equals(nameTag)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * フォルダ配下にタグがあるかどうかチェックします。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param object 対象のフォルダオブジェクト
	 * @param sessHelper EIMSessionの付け替え支援クラス
	 * @param condHelper ドキュメント管理オブジェクト判定の支援クラス
	 * @return trueなら存在する
	 * @throws Exception
	 */
	public static boolean isTagExistUnderFolder(EIMSession sess, EIMObject object, AppObjectConditionHelper condHelper) throws Exception {

		// 配下オブジェクトの取得
		List childList = condHelper.getChildObjectsWithoutAnyConditions(object, false);

		for (Iterator i = childList.iterator(); i.hasNext();) {
			EIMObject tmpObj = (EIMObject) i.next();
			// タグであれば
			if(condHelper.isTypeOfTag(tmpObj.getType())) {
				return true;
			}
			// フォルダであれば
			else if(condHelper.isTypeOfFolder(tmpObj.getType())) {
				// 再帰処理
				if( isTagExistUnderFolder(sess, tmpObj, condHelper) ) {
					return true;
				}
			}
		}
		return false;
	}

	/**
	 * リンクリレーションの属性値「リンク更新タイミング」を設定する。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param relation 対象のリレーション
	 * @param linkUpdateTiming リンク更新タイミング
	 * @throws Exception
	 */
	public static void setAttributeLinkRelation(EIMSession sess, EIMRelation relation, long linkUpdateTiming) throws Exception {

		EIMAttributeType attType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_LINK_UPDATE_TIMING"));
		RelationAttributeUtils.setAttribute(sess, relation, attType, linkUpdateTiming);

		return;
	}

	/**
	 * 親が貼り付け先で、子がchildと同一のリビジョングループのオブジェクトで、リンクリレーションを持つかどうかを確認し、
	 * リレーションが存在すれば重複しているとしてtrueを返却する。重複していない場合はfalseを返却する。
	 *
	 * @param helper ヘルパークラス
	 * @param parent ドキュメントまたはリンクの貼り付け先
	 * @param child 貼り付け対象
	 * @param childObjVersionMap 子オブジェクトIDをキーにして、バージョンIDを値に持つ。
	 * @param parentObjVersionListMap 親オブジェクトIDをキーにして、処理したリビジョン番号のリストを値に持つ。
	 * @return 重複している場合はtrue、重複していない場合はfalseを返却する
	 * @throws Exception
	 */
	public static boolean isDuplicateLinkRelationAuto(AppObjectConditionHelper helper, EIMObject parent, EIMObject child,
			HashMap<Long, Long> childObjVersionMap, HashMap<Long, List> parentObjVersionListMap) throws Exception {

		if( childObjVersionMap!=null && parentObjVersionListMap!=null) {
			// フォルダツリー複製の場合
			// 同一階層内でのバージョンを保持する。

			// 同一リビジョングループが取得されているかをチェックする。
			Long versionId = childObjVersionMap.get((long)child.getId());

			if(versionId == null) {
				// 対象オブジェクトのリビジョングループIDを取得していない
				EIMVersion version = helper.getVersion(child);
				versionId = (long)version.getId();
				List objList = version.getList();
				for(Iterator k = objList.iterator(); k.hasNext(); ) {
					EIMObject versionObj = (EIMObject)k.next();
					childObjVersionMap.put((long)versionObj.getId(), versionId);
				}
			}

			List oldVersionIdList = parentObjVersionListMap.get(parent.getId());
			if( oldVersionIdList == null) {
				// 同一の親オブジェクトに対して未処理
				oldVersionIdList = new ArrayList();
			}
			else {
				// 同一の親オブジェクトに対して処理済み
				if( oldVersionIdList.contains(versionId) ) {
					// 同じ親オブジェクトに対して同一のリビジョングループの処理済み
					return true;
				}
			}
			oldVersionIdList.add(versionId);
			parentObjVersionListMap.put((long)parent.getId(), oldVersionIdList);

			return false;
		}
		else {
			// フォルダツリー複製以外

			// リンクリレーション検索のヘルパー
			EIMSearchSelectEIMRelation.SearchConditionBuildHelper searchRelHelper =
							new EIMSearchSelectEIMRelation.SearchConditionBuildHelper();
			EIMSearchSelectEIMObject.SearchConditionBuildHelper searchObjHelper =
							new EIMSearchSelectEIMObject.SearchConditionBuildHelper();



			//==================================================================================================
			// リレーション検索条件項目・返却項目指定インスタンスを生成
			EIMSearchSelectEIMRelation relationSelectTarget = new EIMSearchSelectEIMRelation();

			// リレーション検索条件グループ作成
			EIMSearchConditionGroup relationConds
							= new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);
			EIMSearchConditionGroup relconds = searchRelHelper.group(searchRelHelper.opAnd());
			relconds.addCondition(searchRelHelper.eq(searchRelHelper.opAnd(),
					EIMSearchSelectEIMRelation.PsedoAttributeTypeEnum.TYPE, helper.getRelationTypeOfDocLink().getId()));
			relationConds.addCondition(relconds);


			//==================================================================================================
			// 親が貼り付け先
			EIMSearchSelectEIMObject parentObjSelectTarget = new EIMSearchSelectEIMObject();

			// オブジェクト検索条件グループ作成
			EIMSearchConditionGroup objConds
							= new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);

			EIMSearchConditionGroup parentObjConds = searchObjHelper.group(searchObjHelper.opAnd());
			objConds.addCondition(searchObjHelper.eq(searchObjHelper.opAnd(),
					EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.ID, parent.getId()));
			parentObjConds.addCondition(objConds);

			//==================================================================================================
			// オブジェクト検索条件グループ作成
			EIMVersion version = VersionUtils.getVersion(helper.getSession(), child);
			List objList = version.getList();
			long[] objIdList = new long[objList.size()];
			for(int i=0; i<objList.size(); i++) {
				EIMObject obj = (EIMObject)objList.get(i);
				objIdList[i] = obj.getId();
			}

			EIMSearchSelectEIMObject childObjSelectTarget = new EIMSearchSelectEIMObject();

			EIMSearchConditionGroup objConds2
							= new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);

			EIMSearchConditionGroup childObjConds = searchObjHelper.group(searchObjHelper.opAnd());
			objConds2.addCondition(searchObjHelper.in(searchObjHelper.opAnd(),
					EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.ID, searchObjHelper.opIn(), TypeConvertUtils.convertToBuildTypeArray(objIdList)));
			childObjConds.addCondition(objConds2);

			// 検索グループに検索条件設定
			relationSelectTarget.setCondition(relationConds);
			parentObjSelectTarget.setCondition(parentObjConds);
			childObjSelectTarget.setCondition(childObjConds);

			// リレーションの取得項目 リンク更新タイミング属性を取得する
			List<EIMAttributeType> attrList = new ArrayList<EIMAttributeType>();
			EIMAttributeType attrType = helper.getAttrType(EIMConfig.get("ATTR_NAME_LINK_UPDATE_TIMING"));
			attrList.add(attrType);
			relationSelectTarget.setResultAttrs(attrList);

			// 検索実行
			EIMSearchResultList searchResultList = SearchUtils.searchRelations(helper.getSession(),
					relationSelectTarget,
					parentObjSelectTarget,
					childObjSelectTarget,
					null);

			// 1件以上存在すれば重複しているとみなす。
			if( searchResultList.size() > 0 ) {
				for(int i=0; i<searchResultList.size(); i++ ) {
					EIMRelation rel = (EIMRelation)searchResultList.get(i);
					EIMAttribute attr = rel.getAttributeByName(EIMConfig.get("ATTR_NAME_LINK_UPDATE_TIMING"));
					if( attr.getInt() == 1 ) {
						return true;
					}
				}
				return false;
			} else {
				return false;
			}
		}

	}

	/**
	 * 過去リビジョンの通知を行うかの設定をチェックする
	 * config_doc.propertiesのDOCUMENT_LINK_LATEST_NOTIFICATIONの設定を確認する。
	 *
	 * @param sess EIMSessionインスタンス
	 * @return trueなら表示する、falseはしない
	 * @throws Exception パラメータがない(NULL)、パラメータ値が"on"と"off"以外
	 */
	public static boolean isDisplayLatestLink(EIMSession sess) throws Exception {

		String str = EIMConfig.get("DOCUMENT_LINK_LATEST_NOTIFICATION");
		if( str != null ) {
			if( str.equals("on") ) {
				return true;
			}
			else if( str.equals("off") ) {
				return false;
			}
		}

		throw new EIMException(sess, "EIM.ERROR.CONFIG.SETTING.ERROR", new Object[]{"DOCUMENT_LINK_LATEST_NOTIFICATION"});
	}

	/**
	 * リンクリレーションのリンク設定を更新します。
	 * @param sess EIMSessionインスタンス
	 * @param objectId ドキュメントリンクを選択した場合はリンクの原本オブジェクト、フォルダを選択した場合はフォルダのオブジェクトID
	 * @param parentObjectId 親オブジェクトのID
	 * @param prmIsDocumentLink ドキュメントリンクのフラグ。"true"ならはドキュメントリンク、それ以外はフォルダとして扱う。
	 * @param prmLinkUpdateTiming 設定するリンク更新タイミング "0"は手動更新、"1"は公開時更新。
	 * @throws Exception
	 */
	public static void setLinkSetting(EIMSession sess, String objectId, String parentObjectId,
			String prmIsDocumentLink, String prmLinkUpdateTiming) throws Exception {

		// 条件判定ヘルパー
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		int linkUpdateTiming = Integer.parseInt(prmLinkUpdateTiming);

		if( prmIsDocumentLink.equals("true") ) {
			// ドキュメントリンクの場合

			// 原本の存在確認と読み込み権限チェック
			EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(objectId));
			if(object == null || !SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.READ))
			{
				throw new EIMException(sess, "EIM.ERROR.LOGIC.NOTFOUNDLINKDEST");
			}

			// Parent Object
			EIMObject parentObject = ObjectUtils.getObjectById(sess, Long.parseLong(parentObjectId));
			if(parentObject == null)
			{
				throw new EIMException(sess, "EIM.ERROR.LOGIC.PARENT.NOTFOUND");
			}

			// リンク先の権限確認。上位フォルダステータス。無ステータスか編集中で無ければNG
			if (parentObject.getStatus() != null
					&& parentObject.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {
				throw new EIMException(sess, "EIM.ERROR.LOGIC.PARENT.NOTUPDATING", new Object[]{object.getName()});
			}

			// リンク先の作成権限
			if (!SecurityUtils.authorized(sess, parentObject, sess.getUser(), EIMAccessRole.CREATE)) {
				throw new EIMException(sess, "EIM.ERROR.LOGIC.NOUPDATELINKSETTINGROLE");
			}

			// ドキュメントリンクのみ有効
			EIMRelation rel = RelationUtils.getRelationByParentAndChild(sess, helper.getRelationTypeOfDocLink() , parentObject, object);
			if( rel == null ) {
				throw new EIMException(sess, "EIM.ERROR.LOGIC.NOTEXIST.DOCLINK");
			}

			// 手動⇒公開時に変更する場合に、同一フォルダ内に同一リビジョングループの公開時更新リンクがある場合はエラー
			// 公開時⇒手動は、チェック不要。
			if( linkUpdateTiming==AppConstant.LINK_UPDATE_TIMING_AUTO && isDuplicateLinkRelationAuto(helper, parentObject, object, null, null) ) {
				throw new EIMException(helper.getSession(), "EIM.ERROR.LOGIC.DOCLINK.AUTOLINK.EXIST");
			}

			RelationAttributeUtils.setAttribute(sess, rel,
					helper.getAttrType(EIMConfig.get("ATTR_NAME_LINK_UPDATE_TIMING")), linkUpdateTiming);

			// Create Operation History
			OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, AppConstant.UPDATE_LINK_SETTING,
				AppConstant.LINK_SOURCE, EIMConstant.OBJECT, object,
				AppConstant.LINK_ORIGIN, EIMConstant.OBJECT, parentObject,
				AppObjectUtil.getPath(object));
		}
		else {
			// フォルダの場合

			// 対象フォルダの存在確認と書き込み権限チェック
			EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(objectId));
			if(object == null || !SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.CREATE))
			{
				throw new EIMException(sess, "EIM.ERROR.LOGIC.NOUPDATELINKSETTINGROLE");
			}

			// 対象フォルダがワークフロー付きフォルダ配下の場合は、編集中以外対象外とする。
			if( object.getStatus() != null &&
					helper.isTypeOfFolderUnderFolderWithWorkflow(object) &&
					object.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING  ) {
				throw new EIMException(sess, "EIM.ERROR.LOGIC.PARENT.NOTUPDATING", new Object[]{object.getName()});
			}

			// フォルダのパスを取得
			EIMAttribute attr = object.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_PASS"));
			String originalPath = attr.getStrings()[0] + object.getName() + '/';

			// オブジェクト・リレーション検索のヘルパー
			EIMSearchSelectEIMRelation.SearchConditionBuildHelper searchRelHelper =
							new EIMSearchSelectEIMRelation.SearchConditionBuildHelper();
			EIMSearchSelectEIMObject.SearchConditionBuildHelper searchObjHelper =
							new EIMSearchSelectEIMObject.SearchConditionBuildHelper();

			// 条件１：リンクリレーションタイプ
			// 条件２：親オブジェクト。書き込み権限を持ち、対象のフォルダまたは配下のフォルダ、かつ、ワークフローなしまたは編集中。
			// 条件３：子オブジェクト。ドキュメントリンクを持ち、属性「パス」が対象フォルダで始まり、読み込み権限を持つ


			// リレーション検索条件項目・返却項目指定インスタンスを生成
			EIMSearchSelectEIMRelation relationSelectTarget = new EIMSearchSelectEIMRelation();

			// リレーション検索条件グループ作成
			// 条件１：リレーションタイプが「リンク」
			EIMSearchConditionGroup relationConds
							= new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);

			EIMSearchConditionGroup relconds = searchRelHelper.group(searchRelHelper.opAnd());
			relconds.addCondition(searchRelHelper.eq(searchRelHelper.opAnd(),
					EIMSearchSelectEIMRelation.PsedoAttributeTypeEnum.TYPE, helper.getRelationTypeOfDocLink().getId()));
			relationConds.addCondition(relconds);
			relationSelectTarget.setCondition(relationConds);

			// リレーションの取得項目 リンク更新タイミング属性を取得する
			List<EIMAttributeType> attrList = new ArrayList<EIMAttributeType>();
			EIMAttributeType attrTypeLinkUpdateTimng = helper.getAttrType(EIMConfig.get("ATTR_NAME_LINK_UPDATE_TIMING"));
			attrList.add(attrTypeLinkUpdateTimng);
			relationSelectTarget.setResultAttrs(attrList);

			// 条件２：親オブジェクト検索条件項目・返却項目指定インスタンスを生成
			EIMSearchSelectEIMObject parentObjSelectTarget = createParentObjSelectTarget(object, helper, originalPath);

			// 条件３：子オブジェクト検索条件項目・返却項目指定インスタンスを生成
			EIMSearchSelectEIMObject childObjSelectTarget = createChildObjSelectTarget(helper, originalPath);

			// 検索実行
			EIMSearchResultList searchResultList = SearchUtils.searchRelations(helper.getSession(),
					relationSelectTarget,
					parentObjSelectTarget,
					childObjSelectTarget,
					null);

			HashMap<Long, Long> childObjMap = new HashMap<Long, Long>();	// オブジェクトIDをキーにしてバージョンIDを格納
			HashMap<Long, List> parentObjMap = new HashMap<Long, List>();			// 親オブジェクトIDをキーにして確認したバージョンIDリストを格納。

			for(Iterator i = searchResultList.iterator(); i.hasNext(); )
			{
				//Child Object
				EIMRelation linkRelation = (EIMRelation)i.next();

				// 自動にする際は、同一リビジョングループのリンクかどうかをチェックする。
				if( linkUpdateTiming == AppConstant.LINK_UPDATE_TIMING_AUTO ) {
					Long versionId = childObjMap.get((long)linkRelation.getChild().getId());

					if(versionId == null) {
						// 対象オブジェクトのリビジョングループIDを取得していない
						EIMVersion version = VersionUtils.getVersion(sess, linkRelation.getChild());
						versionId = (long)version.getId();
						List objList = version.getList();
						for(Iterator k = objList.iterator(); k.hasNext(); ) {
							EIMObject versionObj = (EIMObject)k.next();
							childObjMap.put((long)versionObj.getId(), versionId);
						}
					}

					List oldVersionIdList = parentObjMap.get((long)linkRelation.getParent().getId());
					if( oldVersionIdList == null) {
						// 同一の親オブジェクトに対して未処理
						oldVersionIdList = new ArrayList();
					}
					else {
						// 同一の親オブジェクトに対して処理済み
						if( oldVersionIdList.contains(versionId) ) {
							// 同じ親オブジェクトに対して同一のリビジョングループの処理済み
							throw new EIMException(helper.getSession(), "EIM.ERROR.LOGIC.DOCLINK.AUTOLINK.EXIST");
						}
					}
					oldVersionIdList.add(versionId);
					parentObjMap.put((long)linkRelation.getParent().getId(), oldVersionIdList);
				}

				// リレーションに属性を設定
				RelationAttributeUtils.setAttribute(sess, linkRelation,
						helper.getAttrType(EIMConfig.get("ATTR_NAME_LINK_UPDATE_TIMING")), linkUpdateTiming);

				// Create Operation History
				OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, AppConstant.UPDATE_LINK_SETTING,
					AppConstant.LINK_SOURCE, EIMConstant.OBJECT, linkRelation.getChild(),
					AppConstant.LINK_ORIGIN, EIMConstant.OBJECT, linkRelation.getParent(),
					AppObjectUtil.getPath(linkRelation.getChild()));
			}
		}

	}

	/**
	 * 親オブジェクトの検索条件を作成する。
	 * @param object 選択したフォルダのオブジェクト
	 * @param helper ヘルパークラス
	 * @param originalPath 選択したフォルダまでのパス
	 * @return 親オブジェクト検索条件
	 * @throws Exception
	 */
	private static EIMSearchSelectEIMObject createParentObjSelectTarget(EIMObject object, AppObjectConditionHelper helper, String originalPath) throws Exception {

		//オブジェクト検索条件項目・返却項目指定インスタンスを生成
		EIMSearchSelectEIMObject parentObjSelectTarget = new EIMSearchSelectEIMObject();

		EIMSearchConditionGroup allConds = new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);

		// 検索グループ１：対象フォルダまたは配下のフォルダ
		EIMSearchConditionGroup conds = new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);

		// 条件1: 属性「パス」がフォルダのパスで始まる(前方一致)
		EIMSearchConditionLike cond1 = new EIMSearchConditionLike(
												EIMSearchOperatorEnum.AND
												, helper.getAttrType(EIMConfig.get("ATTR_NAME_DOCUMENT_PASS"))
												, EIMSearchOperatorEnum.LIKE
												, originalPath+"*"
											);
		// 条件1を条件グループに加える
		conds.addCondition(cond1);

		// 条件2: 選択したフォルダ
		EIMSearchConditionCompare cond2 = new EIMSearchConditionCompare(
												EIMSearchOperatorEnum.OR
												, EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.ID
												, EIMSearchOperatorEnum.EQ
												, object.getId()
											);
		// 条件2を条件グループに加える
		conds.addCondition(cond2);

		allConds.addCondition(conds);


		// 検索グループ２：ステータスなし、または、編集中ステータスを持つ
		EIMSearchConditionGroup editConds = new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);

		// 条件3: ステータスなし
		EIMSearchConditionCompare cond3 = new EIMSearchConditionCompare(
				EIMSearchOperatorEnum.OR
				, EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.STATUS
				, EIMSearchOperatorEnum.EQ
				, 0
			);

		// 条件3を条件グループに加える
		editConds.addCondition(cond3);

		// 条件4: 編集中ステータス
		EIMSearchConditionCompare cond4 = new EIMSearchConditionCompare(
				EIMSearchOperatorEnum.OR
				, EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.STATUS_TYPE_KIND
				, EIMSearchOperatorEnum.EQ
				, AppConstant.STATUS_TYPE_KIND_ID_EDITTING
			);

		// 条件4を条件グループに加える
		editConds.addCondition(cond4);

		allConds.addCondition(editConds);

		parentObjSelectTarget.setCondition(allConds);
		parentObjSelectTarget.setRole(EIMAccessRole.CREATE);


		return parentObjSelectTarget;

	}

	/**
	 * 子オブジェクトの検索条件を作成する。
	 * @param helper ヘルパークラス
	 * @param originalPath 選択したフォルダまでのパス
	 * @return 親オブジェクト検索条件
	 * @throws Exception
	 */
	private static EIMSearchSelectEIMObject createChildObjSelectTarget(AppObjectConditionHelper helper, String originalPath) throws Exception {

		EIMSearchSelectEIMObject childObjSelectTarget = new EIMSearchSelectEIMObject();

		EIMSearchConditionGroup childConds = new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);

		// 条件1: 属性「ドキュメントリンク」が1
		EIMSearchConditionCompare childCond1 = new EIMSearchConditionCompare(
												EIMSearchOperatorEnum.AND
												, helper.getAttrType(EIMConfig.get("ATTR_NAME_DOCUMENT_DOCUMENT_LINK"))
												, EIMSearchOperatorEnum.EQ
												, 1
											);
		// 条件1を条件グループに加える
		childConds.addCondition(childCond1);

		// 条件2: 属性「パス」にフォルダパスで始まる属性値あり（前方一致）
		EIMSearchConditionLike childCond2 = new EIMSearchConditionLike(
												EIMSearchOperatorEnum.AND
												, helper.getAttrType(EIMConfig.get("ATTR_NAME_DOCUMENT_PASS"))
												, EIMSearchOperatorEnum.LIKE
												, originalPath+"*"
											);
		// 条件2を条件グループに加える
		childConds.addCondition(childCond2);

		// 条件3：読み込み権限
		childObjSelectTarget.setCondition(childConds);
		childObjSelectTarget.setRole(EIMAccessRole.READ);

		return childObjSelectTarget;
	}


	/**
	 * 子オブジェクトリストを指定してリンクリレーションを取得する。
	 * @param sess EIMSessionインスタンス
	 * @param childObjList 子オブジェクトリスト
	 * @throws Exception
	 */
	public static EIMSearchResultList getLinkRelationByChildObjList(EIMSession sess, List objectList) throws Exception {

		// 条件判定ヘルパー
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		// オブジェクト・リレーション検索のヘルパー
		EIMSearchSelectEIMRelation.SearchConditionBuildHelper searchRelHelper =
						new EIMSearchSelectEIMRelation.SearchConditionBuildHelper();

		// リレーション検索条件項目・返却項目指定インスタンスを生成
		EIMSearchSelectEIMRelation relationSelectTarget = new EIMSearchSelectEIMRelation();

		// リレーション検索条件グループ作成
		// 条件１：リレーションタイプが「リンク」
		EIMSearchConditionGroup relationConds
						= new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);

		EIMSearchConditionGroup relconds = searchRelHelper.group(searchRelHelper.opAnd());
		relconds.addCondition(searchRelHelper.eq(searchRelHelper.opAnd(),
				EIMSearchSelectEIMRelation.PsedoAttributeTypeEnum.TYPE, helper.getRelationTypeOfDocLink().getId()));
		relationConds.addCondition(relconds);
		relationSelectTarget.setCondition(relationConds);

		// リレーションの取得項目 リンク更新タイミング属性を取得する
		List<EIMAttributeType> attrList = new ArrayList<EIMAttributeType>();
		EIMAttributeType attrTypeLinkUpdateTimng = helper.getAttrType(EIMConfig.get("ATTR_NAME_LINK_UPDATE_TIMING"));
		attrList.add(attrTypeLinkUpdateTimng);
		relationSelectTarget.setResultAttrs(attrList);

		// 条件３：子オブジェクト検索条件項目・返却項目指定インスタンスを生成
		EIMSearchSelectEIMObject childObjSelectTarget = new EIMSearchSelectEIMObject();

		EIMSearchConditionGroup childConds = new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);

		long[] childObjIds = new long[objectList.size()];
		for (int i = 0; i < objectList.size(); i++) {
			childObjIds[i] = ((EIMObject)objectList.get(i)).getId();
		}

		// 条件1: 指定のオブジェクトリスト
		EIMSearchConditionIn childCond1 = new EIMSearchConditionIn(
												EIMSearchOperatorEnum.AND
												, EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.ID
												, EIMSearchOperatorEnum.IN
												, TypeConvertUtils.convertToBuildTypeArray(childObjIds)
											);
		// 条件1を条件グループに加える
		childConds.addCondition(childCond1);
		childObjSelectTarget.setCondition(childConds);

		// 検索実行
		EIMSearchResultList searchResultList = SearchUtils.searchRelations(helper.getSession(),
				relationSelectTarget,
				null,
				childObjSelectTarget,
				null);

		return searchResultList;
	}

	/**
	 * Listを指定したサイズ毎に分割する。
	 * @param list
	 * @param size
	 * @return
	 */
	public static List<long[]> devideList(List<Long> list, int size) {

		int block = list.size() / size + (list.size() % size > 0 ? 1 : 0 );
		List<long[]> devidedList = new ArrayList<long[]>(block);
		for (int i = 0; i < block; i ++) {
			int start = i * size;
			int end = Math.min(start + size, list.size());
			// 分割
			List<Long> tempList = list.subList(start, end);
			// 分割したリストを配列に変換
			long[] tempArray = new long[tempList.size()];
			for(int j = 0; j < tempList.size(); j++){
				tempArray[j] = tempList.get(j);
			}
			// 分割結果リストに格納
			devidedList.add(tempArray);
		}
		return devidedList;
	}

}