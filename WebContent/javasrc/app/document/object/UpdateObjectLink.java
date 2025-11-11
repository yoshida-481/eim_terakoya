package app.document.object;

import java.util.List;

import common.util.AppConstant;
import common.util.AppLogicUtil;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectUtil;
import common.util.AppUpdateNoticeUtils;
import eim.bo.EIMAccessRole;
import eim.bo.EIMAttribute;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMRelation;
import eim.bo.EIMRelationType;
import eim.bo.EIMVersion;
import eim.net.EIMSession;
import eim.util.AccessUtils;
import eim.util.EIMConfig;
import eim.util.ObjectUtils;
import eim.util.RelationUtils;
import eim.util.SecurityUtils;
import eim.util.VersionUtils;

/**
 * オブジェクトリンク最新化クラス
 *
 */
@SuppressWarnings("unchecked")
public class UpdateObjectLink {

	/** セッション情報 */
	private EIMSession _sess = null;

	/** リンク先オブジェクトID */
	private long _objectId = -1;

	/** リンク元オブジェクト(フォルダ、ワークスペース)ID */
	private long _parentObjId = -1;

	/** リンク元オブジェクト */
	private EIMObject _object = null;

	/** リンク先オブジェクト */
	private EIMObject _parenObject = null;

	/** リンク先オブジェクトのパス */
	private String _parentPath = null;

	/** 最新の履歴のオブジェクト */
	private EIMObject _latestObj = null;

	private List _linkRelList = null;

	/**
	 * オブジェクトリンク最新化クラスのコンストラクタ
	 * @param sess セッション情報
	 * @param objectId オブジェクトID
	 * @param parentObjId 親オブジェクトのID
	 */
	public UpdateObjectLink(EIMSession sess, long objectId, long parentObjId)
	{
		this.setSession(sess);
		this.setObjectId(objectId);
		this.setParentObjId(parentObjId);
	}

	/**
	 * ドキュメントリンクが存在しているかのチェック
	 * @return 存在していればtrueを返す
	 */
	public boolean isExistDocumentLink()
	throws Exception, EIMException
	{
		List linkList = this.getLinkRelList();

		for (int i = 0; i < linkList.size(); i++)
		{
			EIMRelation relation = (EIMRelation)linkList.get(i);
			EIMObject child = relation.getChild();

			// 一致しているオブジェクトIDがあればリンクは存在している
			if (child.getId() == this.getObjectId())
			{
				return true;
			}
		}

		// 見つからなかった
		return false;
	}

	/**
	 * 同じパスに最新のオブジェクトが被っているかチェック
	 * @param latestObj
	 * @return
	 */
	public boolean latestObjIsSamePath()
	throws Exception, EIMException
	{
		String paths[] = this.getLatestObj().getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_PASS")).getStrings();
		String linkSourcePath = this.getParentPath();

		// リンク元オブジェクトのパスと一致していたらオブジェクトリンクと
		// 同じパスに最新のオブジェクトをリンク先とするオブジェクトリンクが存在する
		for (int i = 1; i < paths.length; i++)
		{
			if (paths[i].compareTo(linkSourcePath) == 0) {
				return true;
			}
		}

		return false;
	}

	/**
	 * オブジェクトのリンクを新しいものに置き換える
	 * @param newLinkObj
	 * @throws Exception
	 * @throws EIMException
	 */
	public void updateLink()
	throws Exception, EIMException
	{
		long linkUpdateTiming = AppConstant.LINK_UPDATE_TIMING_MANUAL;
		
		// ドキュメントパスの削除
		AppLogicUtil.deleteDocLinkPath(this.getSession(), this.getObject(), this.getParentObjId());

		// リレーションの削除
		List parentRelList = this.getLinkRelList();
		for (int i = 0; i < parentRelList.size(); i++) {
			EIMRelation rel = (EIMRelation) parentRelList.get(i);
			if(rel.getChild().getId() == this.getObjectId()) {
				// relは属性値を含んでいないので再取得
				EIMRelation relAll = RelationUtils.getRelationById(this.getSession(), rel.getId());
				EIMAttribute attr = relAll.getAttributeByName(EIMConfig.get("ATTR_NAME_LINK_UPDATE_TIMING"));
				linkUpdateTiming = attr.getInt();
				// 削除されるリレーションは1つだけ
				RelationUtils.deleteRelation(this.getSession(), rel);
				break;
			}
		}

		/* 新しいリンクを作成 */
		AppLogicUtil.createDocLink(this.getLatestObj(), this.getParenObject(),
				false, false, new AppObjectConditionHelper(this.getSession()), linkUpdateTiming);

		// SearchFramework 検索FW更新通知 対象：旧リンク元、新リンク先
		AppUpdateNoticeUtils.updateNoticeInsert(this.getObject().getId(), "SEARCHFW_UPDATEOBJLINK_DOCUMENTLINK");
		AppUpdateNoticeUtils.updateNoticeInsert(this.getLatestObj().getId(), "SEARCHFW_UPDATEOBJLINK_DOCUMENT_LATEST");
		
		// アクセス履歴を登録(オブジェクトリンク最新化)
		AccessUtils.createAccess(this.getSession(), this.getObject(), "EIM.ACCESS.TYPE.DOCLINK.UPDATE");

	}

	/**
	 * オブジェクトのリンクを削除する
	 * @throws Exception
	 * @throws EIMException
	 */
	public void deleteLink() throws Exception, EIMException
	{
		long linkUpdateTiming = AppConstant.LINK_UPDATE_TIMING_MANUAL;
		
		// ドキュメントパスの削除
		AppLogicUtil.deleteDocLinkPath(this.getSession(), this.getObject(), this.getParentObjId());

		// リレーションの削除
		List parentRelList = this.getLinkRelList();
		for (int i = 0; i < parentRelList.size(); i++) {
			EIMRelation rel = (EIMRelation) parentRelList.get(i);
			if(rel.getChild().getId() == this.getObjectId()) {
				// relは属性値を含んでいないので再取得
				EIMRelation relAll = RelationUtils.getRelationById(this.getSession(), rel.getId());
				EIMAttribute attr = relAll.getAttributeByName(EIMConfig.get("ATTR_NAME_LINK_UPDATE_TIMING"));
				linkUpdateTiming = attr.getInt();
				// 削除されるリレーションは1つだけ
				RelationUtils.deleteRelation(this.getSession(), rel);
				break;
			}
		}

		// SearchFramework 検索FW更新通知 対象：旧リンク元、新リンク先
		AppUpdateNoticeUtils.updateNoticeInsert(this.getObject().getId(), "SEARCHFW_UPDATEOBJLINK_DOCUMENTLINK");
		
		// アクセス履歴を登録(オブジェクトリンク削除)
		AccessUtils.createAccess(this.getSession(), this.getObject(), "EIM.ACCESS.TYPE.DOCLINK.DELETE");

	}

	/**
	 * リンク先に権限が存在するかチェックする
	 * @param helper
	 */
	public boolean isRoleLegal()
	throws Exception, EIMException
	{
		long statusKind;

		/* リンク先のチェック */

		// リンク先のステータスチェック（リンク先の読取権限チェックは getObject() 内で行う）
		statusKind = (this.getObject().getStatus() != null) ?
				this.getObject().getStatus().getType().getKind() : AppConstant.STATUS_TYPE_KIND_ID_NONE;
		if (statusKind != AppConstant.STATUS_TYPE_KIND_ID_NONE && statusKind != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC)
		{
			return false;
		}

		/* リンク元のチェック */

		// 作成権限
		if (!SecurityUtils.authorized(this.getSession(), this.getParenObject(),this.getSession().getUser(), EIMAccessRole.CREATE)) {
			return false;
		}

		// ステータスのチェック
		statusKind = (this.getParenObject().getStatus() != null) ?
				this.getParenObject().getStatus().getType().getKind() : AppConstant.STATUS_TYPE_KIND_ID_NONE;
		if (statusKind != AppConstant.STATUS_TYPE_KIND_ID_NONE && statusKind != AppConstant.STATUS_TYPE_KIND_ID_EDITTING)
		{
			return false;
		}

		// 権限に不正無し
		return true;
	}

	/**
	 * DBからオブジェクトの情報セットする
	 * @throws Exception
	 * @throws EIMException
	 */
	public void reloadObject()
	throws Exception, EIMException
	{
		this.setObject(null);
		this.getObject();
	}

	/**
	 * DBから最新履歴のオブジェクトの情報をセットする
	 * @throws Exception
	 * @throws EIMException
	 */
	public void reloadLatestObj()
	throws Exception, EIMException
	{
		this.setLatestObj(null);
		this.getLatestObj();
	}

	/**
	 * リンク先オブジェクトIDを取得する
	 * @return
	 */
	public long getObjectId() {
		return _objectId;
	}

	/**
	 * リンク先オブジェクトIDをセットする
	 * @param _objectId
	 */
	private void setObjectId(long _objectId) {
		this._objectId = _objectId;
	}

	/**
	 * リンク元オブジェクト(フォルダ、ワークスペース)のIDを取得する
	 * @return
	 */
	public long getParentObjId() {
		return _parentObjId;
	}

	/**
	 * リンク元オブジェクト(フォルダ、ワークスペース)のIDを取得する
	 * @param _parentObjId
	 */
	private void setParentObjId(long _parentObjId) {
		this._parentObjId = _parentObjId;
	}

	/**
	 * セッション情報を取得
	 * @return
	 */
	public EIMSession getSession() {
		return _sess;
	}

	/**
	 * セッション情報をセット
	 * @param sess
	 */
	private void setSession(EIMSession sess) {
		this._sess = sess;
	}

	/**
	 *
	 * @return
	 * @throws EIMException
	 * @throws Exception
	 */
	public EIMObject getObject()
	throws EIMException, Exception
	{
		// オブジェクトが未取得であれば新しく取得する
		if (this._object == null) {
			this.setObject(ObjectUtils.getObjectById(this.getSession(), this.getObjectId()));

			// オブジェクトが取得できなかった(リンク先が存在しない)
			EIMSession sess = this.getSession();
			if (this._object == null || !SecurityUtils.authorized(sess, this._object, sess.getUser(), EIMAccessRole.READ)) {
				throw new EIMException(sess, "EIM.ERROR.LOGIC.NOTFOUNDLINKDEST");
			}
		}

		return _object;
	}

	/**
	 * オブジェクトをセット
	 * @param object
	 */
	private void setObject(EIMObject object) {
		this._object = object;
	}

	/**
	 * リンク先オブジェクトの親オブジェクトを取得
	 * @return リンク元オブジェクト
	 * @throws Exception
	 * @throws EIMException
	 */
	public EIMObject getParenObject()
	throws Exception, EIMException
	{
		// オブジェクトが未取得であればDBから取得
		if (this._parenObject == null)
		{
			this.setParenObject(ObjectUtils.getObjectById(this.getSession(), this.getParentObjId()));

			// オブジェクトが取得できなかった(リンク元が存在しない)
			EIMSession sess = this.getSession();
			if (this._parenObject == null || !SecurityUtils.authorized(sess, this._parenObject, sess.getUser(), EIMAccessRole.READ)) {
				throw new EIMException(sess, "EIM.ERROR.LOGIC.NOTFOUNDLINKSOURCE");
			}
		}
		return _parenObject;
	}

	/**
	 * 親オブジェクトのセット
	 * @param parenObject
	 */
	private void setParenObject(EIMObject parenObject) {
		this._parenObject = parenObject;
	}

	/**
	 * オブジェクトリンクの一覧を取得する
	 * @return
	 * @throws EIMException
	 * @throws Exception
	 */
	public List getLinkRelList()
	throws EIMException, Exception
	{

		// リレーションが未取得であれば新しく取得
		if (this._linkRelList == null)
		{
			EIMRelationType type = RelationUtils.getRelationTypeByName(this.getSession(), EIMConfig.get("RELATION_TYPE_NAME_LINK"));
			this.setLinkRelList(RelationUtils.getChildRelationListByRelType(this.getSession(), this.getParenObject(), type,EIMAccessRole.READ));
		}

		return _linkRelList;
	}

	/**
	 * オブジェクトリンクの一覧をセットする
	 * @param linkRelList
	 */
	private void setLinkRelList(List linkRelList) {
		this._linkRelList = linkRelList;
	}

	/**
	 * 最新の履歴を持つドキュメントを取得する
	 * @return the latestObj
	 */
	public EIMObject getLatestObj()
	throws Exception, EIMException
	{
		// 最新履歴のオブジェクトが取得できていなければ新しく取得
		if (this._latestObj == null)
		{
			EIMVersion version;
			version = VersionUtils.getVersion(this.getSession(), this.getObject());
			
			// LATESTフラグが立っているものの中で最もリビジョンが高いバージョンのオブジェクトを取得する
			EIMObject object = null;
			for (int i = version.getList().size() -1; i >= 0; i--) {
				object = (EIMObject)version.getList().get(i);
				
				// ステータスが「なし」もしくは「公開中」である必要がある
				long statusKind = (object.getStatus() != null) ? object.getStatus().getType().getKind() : AppConstant.STATUS_TYPE_KIND_ID_NONE;
				if (statusKind == AppConstant.STATUS_TYPE_KIND_ID_NONE || statusKind == AppConstant.STATUS_TYPE_KIND_ID_PUBLIC) {
					break;
				}
			}
			
			this.setLatestObj(object);
		}

		return _latestObj;
	}

	/**
	 * 最新の履歴を持つドキュメントを設定する
	 * @param latestObj the latestObj to set
	 */
	private void setLatestObj(EIMObject latestObj) {
		this._latestObj = latestObj;
	}

	/**
	 * @return the parentPath
	 */
	public String getParentPath()
	throws EIMException, Exception
	{
		// パスが取得できていなければ新しく取得
		if (this._parentPath == null)
		{
			if (this.getParenObject().getType().getDefName().equals(
					EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACE")))
			{
				// ワークスペース
				this.setParentPath("/" + this.getParenObject().getName() + "/");
			}
			else
			{
				// フォルダ
				this.setParentPath(AppObjectUtil.getPath(this.getParenObject()) +
						this.getParenObject().getName() + "/");
			}
		}

		return _parentPath;
	}

	/**
	 * @param parentPath the parentPath to set
	 */
	private void setParentPath(String parentPath) {
		this._parentPath = parentPath;
	}

}
