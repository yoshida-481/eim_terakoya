package common.bo;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import common.util.AppConstant;
import common.util.AppLogicUtil;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectUtil;
import eim.bo.EIMAccessRole;
import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMFormat;
import eim.bo.EIMObject;
import eim.bo.EIMRelation;
import eim.bo.EIMRelationType;
import eim.net.EIMSession;
import eim.util.DateUtils;
import eim.util.EIMConfig;
import eim.util.RelationUtils;

/**
 * 
 * バージョンノードクラス
 *
 */
public class VersionNode {
	
	/**
	 * オブジェクト
	 */
	private EIMObject obj = null;
	
	/**
	 * オブジェクトID
	 */
	private long objId = Integer.MIN_VALUE;
	
	/**
	 * オブジェクト名
	 */
	private String objName = null;
	
	/**
	 * オブジェクトタイプID
	 */
	private long objTypeId = Integer.MIN_VALUE;
	
	/**
	 * オブジェクトタイプ名
	 */
	private String objTypeName = null;
	
	/**
	 * バージョン番号
	 */
	private int rev = Integer.MIN_VALUE;
	
	/**
	 * 更新者名
	 */
	private String modifyUserName = null;
	
	/**
	 * 更新日時
	 */
	private Date modifyTime = null;
	
	/**
	 * 改訂内容
	 */
	private String updateComment = null;
	
	/**
	 * パス
	 */
	private String path = null;
	
	/**
	 * ステータスタイプ種別
	 */
	private long statusTypeKind = Integer.MIN_VALUE;
	
	/**
	 * WFなしドキュメントの公開アイコン表示フラグ
	 */
	private boolean isDspPubIconForNoWF = false;
	
	/**
	 * PDF結合処理失敗フラグ
	 */
	private boolean isPDFJoinFailed = false;
		
	/**
	 * PDF公開フラグ
	 */
	private boolean isDspPdfIcon = false;
	
	/**
	 * 有効期限切れフラグ
	 */
	private boolean expiration = false;
	
	/**
	 * 読み取り権限のみフラグ
	 */
	private boolean readOnly = false;
	
	/**
	 * デフォルトノードフラグ
	 */
	private boolean defaultFlag = false;
	
	/**
	 * 初期選択フラグ
	 */
	private boolean initialSelect = false;
	
	/**
	 * 子ブランチ有無フラグ
	 */
	private boolean hasChild = false;

	/**
	 * 子ブランチ探索済みフラグ
	 */
	private boolean isSearch = false;
	
	/**
	 * 子ブランチ情報リスト
	 */
	private List childBranchList = new ArrayList();
	
	/**
	 * 署名・暗号化状態
	 */
	private long signencr = AppConstant.SIGNENCR_KIND_NOT_SIGNENCR;
	
	/**
	 * OCR結果ステータス
	 */
	private long ocrResultStatus = AppConstant.OCR_RESULT_STATUS_NONE;
	
	/**
	 * コンストラクタ
	 *
	 */
	public VersionNode() {
	}
	
	/**
	 * オブジェクト情報設定処理
	 * 
	 * @param sess セッション情報
	 * @param object 設定するオブジェクト情報
	 * @param helper 条件判定ヘルパー作成
	 * @param formatPDF 公開ドキュメントのフォーマット
	 * @param relTypeBranch ブランチリレーションタイプ
	 * @param attTypeDelFlag 削除フラグ属性タイプ
	 * @throws Exception
	 */
	public List setObjectInfo(
			EIMSession sess, 
			EIMObject object, 
			AppObjectConditionHelper helper, 
			EIMFormat formatPDF,
			EIMRelationType relTypeBranch,
			EIMAttributeType attTypeDelFlag) throws Exception {

		// オブジェクト
		this.setObj(object);
		// オブジェクトID
		this.setObjId(object.getId());
		// オブジェクト名
		this.setObjName(object.getName());
		// オブジェクトタイプID
		this.setObjTypeId(object.getType().getId());
		// オブジェクトタイプ名
		this.setObjTypeName(object.getType().getDefName());
		// バージョン番号
		this.setRev(object.getRev());
		// 更新者名
		this.setModifyUserName(object.getModifyUser().getName());
		// 更新日時
		this.setModifyTime(object.getModifyDate());
		// 改訂内容
		EIMAttribute attrDocumentRevComment = object.getAttribute(helper.getAttrNameOfUpdateRevComment());
		if(attrDocumentRevComment != null){
			this.setUpdateComment(attrDocumentRevComment.getStrings()[0]);
		}

		// PDF結合処理失敗フラグ
		long joinFailFlg = AppObjectUtil.getIntAttr(sess, object, helper.getAttrNameOfPDFJoinFail(), AppConstant.FLAG_OFF);
		if(joinFailFlg == AppConstant.FLAG_ON) {
			this.setPDFJoinFailed(true);
		}
		else {
			this.setPDFJoinFailed(false);
		}
		
		// 公開フラグ
		if(object.getStatus() != null) {
			this.setStatusTypeKind(object.getStatus().getType().getKind());
			this.setDspPubIconForNoWF(false);
		}
		else {
			if( this.isPDFJoinFailed() ) {	// PDF結合失敗のときは表示しない
				this.setDspPubIconForNoWF(false);				
			}
			else {
				this.setDspPubIconForNoWF(true);				
			}
		}
		
		// PDFアイコン表示判定
		this.setDspPdfIcon(AppLogicUtil.isDspPdfIcon(sess, object, formatPDF));
		
		// 有効期限切れ判定
		boolean expiration = false;
		EIMAttribute expirationDate = object.getAttribute(helper.getAttrNameOfEffectDate());
		if(expirationDate != null)
		{
			expiration = DateUtils.judgeExpirationDate(sess, expirationDate.getDate());
		}
		this.setExpiration(expiration);

		// 読み取り権限のみ判定
		this.setReadOnly(helper.isReadOnlyAccess(object));
		
		// 署名・暗号化状態
		this.setSignencr(AppObjectUtil.getIntAttr(sess, object, helper.getAttrNameOfSignEncStatus(), AppConstant.SIGNENCR_KIND_NOT_SIGNENCR));
		
		// OCR結果ステータス
		EIMAttribute attributeOcrResultStatus = object.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_OCR_RESULT_STATUS"));
		if(attributeOcrResultStatus != null){
			this.setOcrResultStatus(attributeOcrResultStatus.getInt());
		}else{
			this.setOcrResultStatus(AppConstant.OCR_RESULT_STATUS_NONE);
		}
		
		// 子ブランチ有無フラグ
		List relList = RelationUtils.getChildRelationListByRelType(sess, object, relTypeBranch,EIMAccessRole.READ);
		boolean hasChild = false;
		for(int i = 0; i < relList.size() ; i++){
			EIMRelation childRel = (EIMRelation)relList.get(i);
			EIMAttribute delFlag = childRel.getAttributeById(attTypeDelFlag.getId());
			if(	delFlag == null ||
				delFlag.getInts()[0] == AppConstant.FLAG_OFF){
				// 有効な子リレーションがあれば子ブランチあり
				hasChild = true;
				break;
			}
		}
		
		this.setHasChild(hasChild);

		return relList;
	}
	
	/**
	 * 子ブランチリスト取得処理
	 * 
	 * @return 子ブランチ(VersionBranch)のリスト
	 */
	public List getChildBranchList() {
		return childBranchList;
	}

	/**
	 * 子ブランチリスト設定処理
	 * 
	 * @param childBranchList 子ブランチ(VersionBranch)のリスト
	 */
	public void setChildBranchList(List childBranchList) {
		this.childBranchList = childBranchList;
	}

	/**
	 * デフォルトフラグ取得処理
	 * 
	 * @return デフォルトフラグ
	 */
	public boolean isDefaultFlag() {
		return defaultFlag;
	}

	/**
	 * デフォルトフラグ設定処理
	 * 
	 * @param defaultFlag デフォルトフラグ
	 */
	public void setDefaultFlag(boolean defaultFlag) {
		this.defaultFlag = defaultFlag;
	}

	/**
	 * 有効期限切れフラグ取得処理
	 * 
	 * @return 有効期限切れフラグ
	 */
	public boolean isExpiration() {
		return expiration;
	}

	/**
	 * 有効期限切れフラグ設定処理
	 * 
	 * @param expiration 有効期限切れフラグ
	 */
	public void setExpiration(boolean expiration) {
		this.expiration = expiration;
	}

	/**
	 * 子ブランチ有無フラグ取得処理
	 * 
	 * @return 子ブランチ有無フラグ
	 */
	public boolean isHasChild() {
		return hasChild;
	}

	/**
	 * 子ブランチ有無フラグ設定処理
	 * 
	 * @param hasChild 子ブランチ有無フラグ
	 */
	public void setHasChild(boolean hasChild) {
		this.hasChild = hasChild;
	}

	/**
	 * 初期選択フラグ取得処理
	 * 
	 * @return 初期選択フラグ
	 */
	public boolean isInitialSelect() {
		return initialSelect;
	}

	/**
	 * 初期選択フラグ設定処理
	 * 
	 * @param initialSelect 初期選択フラグ
	 */
	public void setInitialSelect(boolean initialSelect) {
		this.initialSelect = initialSelect;
	}

	/**
	 * PDF公開フラグ取得処理
	 * 
	 * @return PDF公開フラグ
	 */
	public boolean isDspPdfIcon() {
		return isDspPdfIcon;
	}

	/**
	 * PDF公開フラグ設定処理
	 * 
	 * @param isDspPdfIcon PDF公開フラグ
	 */
	public void setDspPdfIcon(boolean isDspPdfIcon) {
		this.isDspPdfIcon = isDspPdfIcon;
	}

	/**
	 * 子ブランチ探索済みフラグ取得処理
	 * 
	 * @return 子ブランチ探索済みフラグ
	 */
	public boolean isSearch() {
		return isSearch;
	}

	/**
	 * 子ブランチ探索済みフラグ設定処理
	 * 
	 * @param isSearch 子ブランチ探索済みフラグ
	 */
	public void setSearch(boolean isSearch) {
		this.isSearch = isSearch;
	}

	/**
	 * 更新日時取得処理
	 * 
	 * @return 更新日時
	 */
	public Date getModifyTime() {
		return modifyTime;
	}

	/**
	 * 更新日時設定処理
	 * 
	 * @param modifyTime 更新日時
	 */
	public void setModifyTime(Date modifyTime) {
		this.modifyTime = modifyTime;
	}

	/**
	 * 更新者名取得処理
	 * 
	 * @return 更新者名
	 */
	public String getModifyUserName() {
		return modifyUserName;
	}

	/**
	 * 更新者名設定処理
	 * 
	 * @param modifyUserName 更新者名
	 */
	public void setModifyUserName(String modifyUserName) {
		this.modifyUserName = modifyUserName;
	}

	/**
	 * オブジェクトID取得処理
	 * 
	 * @return オブジェクトID
	 */
	public long getObjId() {
		return objId;
	}

	/**
	 * オブジェクトID設定処理
	 * 
	 * @param objId オブジェクトID
	 */
	public void setObjId(long objId) {
		this.objId = objId;
	}

	/**
	 * オブジェクト名取得処理
	 * 
	 * @return オブジェクト名
	 */
	public String getObjName() {
		return objName;
	}

	/**
	 * オブジェクト名設定処理
	 * 
	 * @param objName オブジェクト名
	 */
	public void setObjName(String objName) {
		this.objName = objName;
	}

	/**
	 * 読み取り権限のみフラグ取得処理
	 * 
	 * @return 読み取り権限のみフラグ
	 */
	public boolean isReadOnly() {
		return readOnly;
	}

	/**
	 * 読み取り権限のみフラグ設定処理
	 * 
	 * @param readOnly 読み取り権限のみフラグ
	 */
	public void setReadOnly(boolean readOnly) {
		this.readOnly = readOnly;
	}

	/**
	 * 署名・暗号化状態取得処理
	 * 
	 * @return 署名・暗号化状態
	 */
	public long getSignencr() {
		return signencr;
	}

	/**
	 * 署名・暗号化状態設定処理
	 * 
	 * @param signencr 署名・暗号化状態
	 */
	public void setSignencr(long signencr) {
		this.signencr = signencr;
	}

	/**
	 * OCR結果ステータス取得処理
	 * 
	 * @return OCR結果ステータス
	 */
	public long getOcrResultStatus() {
		return ocrResultStatus;
	}

	/**
	 * OCR結果ステータス設定処理
	 * 
	 * @param ocrResultStatus OCR結果ステータス
	 */
	public void setOcrResultStatus(long ocrResultStatus) {
		this.ocrResultStatus = ocrResultStatus;
	}
	
	/**
	 * バージョン番号取得処理
	 * 
	 * @return バージョン番号
	 */
	public int getRev() {
		return rev;
	}

	/**
	 * バージョン番号設定処理
	 * 
	 * @param rev バージョン番号
	 */
	public void setRev(int rev) {
		this.rev = rev;
	}

	/**
	 * 改訂内容取得処理
	 * 
	 * @return 改訂内容
	 */
	public String getUpdateComment() {
		return updateComment;
	}

	/**
	 * 改訂内容設定処理
	 * 
	 * @param updateComment 改訂内容
	 */
	public void setUpdateComment(String updateComment) {
		this.updateComment = updateComment;
	}

	/**
	 * パスの取得処理
	 * 
	 * @return パス
	 */
	public String getPath() {
		return path;
	}

	/**
	 * パスの設定処理
	 * 
	 * @param path パス
	 */
	public void setPath(String path) {
		this.path = path;
	}

	/**
	 * オブジェクトタイプID取得処理
	 * 
	 * @return オブジェクトタイプID
	 */
	public long getObjTypeId() {
		return objTypeId;
	}

	/**
	 * オブジェクトタイプID設定処理
	 * 
	 * @param objTypeId オブジェクトタイプID
	 */
	public void setObjTypeId(long objTypeId) {
		this.objTypeId = objTypeId;
	}

	/**
	 * オブジェクトタイプ名取得処理
	 * 
	 * @return オブジェクトタイプ名
	 */
	public String getObjTypeName() {
		return objTypeName;
	}

	/**
	 * オブジェクトタイプ名設定処理
	 * 
	 * @param objTypeName オブジェクトタイプ名
	 */
	public void setObjTypeName(String objTypeName) {
		this.objTypeName = objTypeName;
	}

	/**
	 * WFなしドキュメントの公開アイコン表示フラグ取得処理
	 * 
	 * @return WFなしドキュメントの公開アイコン表示フラグ
	 */
	public boolean isDspPubIconForNoWF() {
		return isDspPubIconForNoWF;
	}

	/**
	 * WFなしドキュメントの公開アイコン表示フラグ設定処理
	 * 
	 * @param isDspPubIconForNoWF WFなしドキュメントの公開アイコン表示フラグ
	 */
	public void setDspPubIconForNoWF(boolean isDspPubIconForNoWF) {
		this.isDspPubIconForNoWF = isDspPubIconForNoWF;
	}

	/**
	 * ステータスタイプ種別取得処理
	 * 
	 * @return ステータスタイプ種別
	 */
	public long getStatusTypeKind() {
		return statusTypeKind;
	}
	
	/**
	 * ステータスタイプ種別設定処理
	 * 
	 * @param statusTypeKind ステータスタイプ種別
	 */
	public void setStatusTypeKind(long statusTypeKind) {
		this.statusTypeKind = statusTypeKind;
	}

	/**
	 * PDF結合処理失敗フラグ取得処理
	 * 
	 * @return PDF結合処理失敗
	 */
	public boolean isPDFJoinFailed() {
		return isPDFJoinFailed;
	}

	/**
	 * PDF結合処理失敗フラグ設定処理
	 * 
	 * @param isPDFJoinFailed PDF結合処理失敗フラグ
	 */
	public void setPDFJoinFailed(boolean isPDFJoinFailed) {
		this.isPDFJoinFailed = isPDFJoinFailed;
	}

	/**
	 * EIMオブジェクト取得
	 * @return
	 */
	public EIMObject getObj() {
		return obj;
	}

	/**
	 * EIMオブジェクトをセット
	 * @param obj
	 */
	public void setObj(EIMObject obj) {
		this.obj = obj;
	}

}
