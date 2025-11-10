package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.io.File;
import java.io.StringWriter;
import java.io.Writer;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import javax.xml.parsers.DocumentBuilder;

import jakarta.servlet.UnavailableException;

import org.apache.catalina.util.DOMWriter;
import org.apache.catalina.util.XMLWriter;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.security.web.savedrequest.FastHttpDateFormat;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import common.util.AppConstant;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectUtil;
import common.util.AppUpdateNoticeUtils;
import common.util.OptionConfData;
import eim.bo.EIMAccessRole;
import eim.bo.EIMAttributeType;
import eim.bo.EIMFormat;
import eim.bo.EIMObject;
import eim.bo.EIMVersion;
import eim.net.EIMSession;
import eim.util.AccessUtils;
import eim.util.AttributeUtils;
import eim.util.EIMConstant;
import eim.util.FileUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.OperationHistoryUtils;
import eim.util.StringUtils;
import eim.util.VersionUtils;
import jp.co.ctc_g.eim.app.document.business.service.DocumentWebDAVService;
import jp.co.ctc_g.eim.app.document.common.util.AppWorkFlowUtil;
import jp.co.ctc_g.eim.framework2.business.dao.FileDao;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AccessRoleTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.FormatDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.WebDAVObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.WorkflowDomain;
import jp.co.ctc_g.eim.framework2.business.service.AttributeTypeService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectTypeService;
import jp.co.ctc_g.eim.framework2.business.service.WorkflowService;
import jp.co.ctc_g.eim.framework2.business.service.impl.WebDAVServiceImpl;
import jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.common.context.TransactionContext;
import jp.co.ctc_g.eim.framework2.common.exception.EIMApplicationException;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;
import jp.co.ctc_g.eim.framework2.common.util.ResourceUtils;
import jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils;
import jp.co.ctc_g.eim.framework2.integration.util.internal.FileUtil;
import jp.co.ctc_g.eim.framework2.presentation.web.servlet.WebDAVStatus;


/**
 * DocumentWebDAVService実装クラス<br>
 * <br>
 * @see jp.co.ctc_g.eim.framework2.business.service.WebDAVService
 * @since Ver6.0
 */
public class DocumentWebDAVServiceImpl extends WebDAVServiceImpl implements DocumentWebDAVService {

	/** MD5メッセージダイジェストアルゴリズム */
	protected static MessageDigest md5Helper;
	/** デフォルトのネームスペース */
	protected static final String DEFAULT_NAMESPACE 	= "DAV:";

	/** ログ出力のためのロガー */
	private Log log = LogFactory.getLog(DocumentWebDAVServiceImpl.class);

	/** 属性タイプサービス */
	private AttributeTypeService attributeTypeService = null;

	/** オブジェクトタイプサービス */
	private ObjectTypeService objectTypeService = null;

	/** ワークフローサービス */
	private WorkflowService workflowService = null;

	/** ファイルDao */
	private FileDao fileDao = null;

	/** depthのデフォルト値 */
	private static final int INFINITY					= 3;
	/** ロックタイムアウトの最大値 */
	private static final int MAX_TIMEOUT				= 604800;
	/** ロック生成 */
	private static final int LOCK_CREATION				= 0;
	/** ロック解除 */
	private static final int LOCK_REFRESH				= 1;

	/** メッセージコード：承認中チェックイン不可 */
	private static final String MSG_CODE_DISABLE_APPROVER_CHECKIN = "EIM.ERROR.LOGIC.DISABLE.APPROVER.CHECKIN";
	/** メッセージコード：承認者以外のチェックイン */
	private static final String MSG_CODE_CANNOT_CHECKIN_ONLY_APPROVER = "EIM.ERROR.LOGIC.CANNOT.CHECKIN.ONLY.APPROVER";

	/** ロックタイムアウトのデフォルト値 */
	public static int defaultTimeout = 0;

	public DocumentWebDAVServiceImpl() throws Exception {
		int defaultValue = 180;
		String value = ConfigUtils.getByKey("WEBDAV_LOCK_TIMEOUT");
		// 分から秒に変換してセットする
		if (value != null) {
			defaultTimeout = Integer.parseInt(value) * 60;
		} else {
			defaultTimeout = defaultValue * 60;
		}

	}

	/**
	 * @see jp.co.ctc_g.eim.framework2.business.service.WebDAVService#doGet(jp.co.ctc_g.eim.framework2.business.domain.entity.WebDAVObjectDomain)
	 * @since Ver6.0
	 */
	@Override
	public void doGet(WebDAVObjectDomain webDAVObjectDomain) throws EIMApplicationException, Exception{

		if(!isDocument(webDAVObjectDomain.getId())) {

			// ドキュメント以外の場合、親クラスの処理のみ実行
			super.doGet(webDAVObjectDomain);

		} else {

			// EIMSession
			EIMSession sess = jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.getEIMSession();

			AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

			EIMObject object = ObjectUtils.getObjectById(sess, webDAVObjectDomain.getId());

			// 権限チェック
			// ステータス的にアクセス可能で、かつ読み取りのみ権限ユーザーではないこと
			if(!(helper.checkAccessibleStatusSelf(object, false) && !helper.isReadOnlyAccess(object))){
				log.error(ResourceUtils.getByKey("EIM.ERROR.LOGIC.NOREADROLE") + "[" + webDAVObjectDomain.getId() + " : " + webDAVObjectDomain.getName() + "] [" + sess.getUser().getCode() + " : " + sess.getUser().getName() + "]");
				throw new EIMApplicationException(WebDAVStatus.SC_FORBIDDEN);
			}

			super.doGet(webDAVObjectDomain);

			// Get後処理
			doGetPost(sess, object);
		}
	}

	private void doGetPost(EIMSession sess, EIMObject object) throws EIMApplicationException, Exception{

		// Access
		AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.DOWNLOAD");

		// パス
		String path = StringUtils.nullToBlank(AppObjectUtil.getPath(object));

		// Create Operation History
		OperationHistoryUtils.create(sess, common.util.AppConstant.DOCUMENT, EIMConstant.DOWNLOAD_DOCUMENT,
				EIMConstant.TARGET_DOWNLOAD, EIMConstant.OBJECT, object,
				null, null, null, path);

		// SearchFramework 検索FW更新通知 対象：ドキュメント
		AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_CHECK_IN_DOCUMENT");
	}

	/**
	 * @see jp.co.ctc_g.eim.framework2.business.service.WebDAVService#doPut(jp.co.ctc_g.eim.framework2.business.domain.entity.WebDAVObjectDomain)
	 * @since Ver6.0
	 */
	@Override
	public void doPut(WebDAVObjectDomain webDAVObjectDomain) throws EIMApplicationException, Exception{

		if(!isDocument(webDAVObjectDomain.getId())) {

			// ドキュメント以外の場合、親クラスの処理のみ実行
			super.doPut(webDAVObjectDomain);

		} else {

			// DBロックをかける
			ObjectDomain object = getObjectService().getByIdForUpdate(webDAVObjectDomain.getId());

			// 原本ファイルを更新する
			super.doPut(webDAVObjectDomain);

			//原本ファイルFormat取得
			FormatDomain orgFormat = getFormatService().getDefaultByObjectType(object.getType());
			if(orgFormat == null) {
				// ドキュメントタイプ[{0}]のフォーマットが取得できませんでした。
				log.error(ResourceUtils.getByKey("EIM.ERROR.LOGIC.NODEFAULTFORM.WITHTYPENAME", new Object[]{object.getType().getName()}));
				throw new EIMApplicationException(WebDAVStatus.SC_FORBIDDEN);
			}

			//原本ファイル取得
			File orgFile = getOriginalFile(object, orgFormat);

			EIMSession sess = jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.getEIMSession();
			EIMObject eimObj = ConvertUtils.toEIMObject(object);

			// ファイルサイズを書き換えて更新
			EIMFormat orgEimFormat = ConvertUtils.toEIMFormat(orgFormat);
			FileUtils.checkin(sess, eimObj, orgEimFormat, object.getName(), orgFile.length());

			// ワークフロー無しの場合、公開ファイルを更新
			if (object.getStatus() == null) {
				//公開ファイルFormat取得
				FormatDomain pubFormat = getFormatService().getByDefinitionName(ConfigUtils.getByKey("FORMAT_NAME_PUBLIC"));
				if(pubFormat == null) {
					throw new EIMApplicationException(WebDAVStatus.SC_FORBIDDEN);
				}

				//公開ファイル取得
				File pubFile = getPublicFile(object, pubFormat);

				//公開ファイルが実ファイルの場合は、メソッド内でシンボリックリンクに置き換える
				FileUtils.createSymbolicLink(orgFile, pubFile);

				if (pubFile != null) {
					// ファイルサイズを書き換えて更新
					EIMFormat pubEimFormat = ConvertUtils.toEIMFormat(pubFormat);
					FileUtils.checkin(sess, eimObj, pubEimFormat, object.getName(), pubFile.length());
				}
			}

			// Put後処理
			doPutPost(object.getId());
		}
	}

	private void doPutPost(long objectId) throws EIMApplicationException, Exception{

		// EIMSession
		EIMSession sess = jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.getEIMSession();


		EIMObject object = ObjectUtils.getObjectById(sess, objectId);

		// フォーマットを取得
		ObjectDomain objectDomain = new ObjectDomain(objectId);
		ObjectTypeDomain objType = new ObjectTypeDomain(object.getType().getId());
		objType.setDefinitionName(object.getType().getDefaultName());
		FormatDomain format = getFormatService().getDefaultByObjectType(objType);

		// ファイルドメインを取得
		FileDomain fileDomain = fileDao.getByObjectAndFormatForUpdate(objectDomain, format);

		boolean isApproverCheckin = false;
		// 承認依頼中チェックイン可能オプションがONの場合アクセス履歴の文言に(承認中)を加える
		if (OptionConfData.getInstance().enableApproverCheckin == true) {
			if(object.getStatus() != null){
				// WFありの場合
				long statusTypeKindId = object.getStatus().getType().getKind();
				// 現在のステータスが承認依頼中の場合
				if (statusTypeKindId == AppConstant.STATUS_TYPE_KIND_ID_APPROVE) {
					isApproverCheckin = true;
				}
			}
		}
		String accType = "EIM.ACCESS.TYPE.CHECKIN";
		String opeType = EIMConstant.CHECKIN;
		if (isApproverCheckin) {
			accType = "EIM.ACCESS.TYPE.CHECKIN.APPROVER";
			opeType = AppConstant.CHECKIN_APPROVER;
		}

		// Access
		AccessUtils.createAccess(sess, object, accType);

		// パス
		String path = StringUtils.nullToBlank(AppObjectUtil.getPath(object));

		// SearchFramework 検索FW更新通知 対象：ドキュメント
		AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_CHECK_IN_DOCUMENT");

		// Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, opeType,
				EIMConstant.TARGET_UPDATE, EIMConstant.OBJECT, object,
				null, null, null, path);
	}

	/**
	 * @see jp.co.ctc_g.eim.framework2.business.service.WebDAVService#doLock(jp.co.ctc_g.eim.framework2.business.domain.entity.WebDAVObjectDomain)
	 * @since Ver6.0
	 */
	@Override
	public void doLock(WebDAVObjectDomain webDAVObjectDomain) throws EIMApplicationException, Exception{

		if(!isDocument(webDAVObjectDomain.getId())) {

			// ドキュメント以外の場合、親クラスの処理のみ実行
			super.doLock(webDAVObjectDomain);

		} else {

			// ログインユーザ
			TransactionContext tx = EIMThreadContext.getTransactionContext();
			UserDomain loginUser = tx.getUser();

			// DBロックをかける
			ObjectDomain object = getObjectService().getByIdForUpdate(webDAVObjectDomain.getId());

			// 書き込み権限判定
			if (!getObjectService().authorized(loginUser, object, new AccessRoleTypeDomain(EIMAccessRole.UPDATE))) {
				// チェックイン権限がありません
				log.error(ResourceUtils.getByKey("EIM.ERROR.LOGIC.NOCHECKINROLE") + "[" + object.getId() + " : " +object.getName() + "] [" + loginUser.getCode()+ " : " +loginUser.getName() + "]");
				throw new EIMApplicationException(WebDAVStatus.SC_FORBIDDEN);
			}

			ObjectTypeDomain objectType = objectTypeService.getById(object.getType().getId());

			// 状態チェック
			String errMsgCode = checkStatusExec(object);
			if (!org.apache.commons.lang3.StringUtils.isEmpty(errMsgCode)) {
				Object[] msgParamArr = null;
				if (MSG_CODE_DISABLE_APPROVER_CHECKIN.equals(errMsgCode)
					|| MSG_CODE_CANNOT_CHECKIN_ONLY_APPROVER.equals(errMsgCode)) {
					msgParamArr = new Object[]{object.getName(), ResourceUtils.getByKey("EIM.ACCESS.TYPE.EDIT")};
				} else {
					msgParamArr = new Object[]{object.getName()};
				}
				log.error(ResourceUtils.getByKey(errMsgCode, msgParamArr));
				throw new EIMApplicationException(WebDAVStatus.SC_FORBIDDEN);
			}

			// ロックユーザチェック
			// ログインユーザがロックしているドキュメントの場合は編集可能
			errMsgCode = checkLockInfo(object);
			if (!org.apache.commons.lang3.StringUtils.isEmpty(errMsgCode)) {
				log.error(ResourceUtils.getByKey(errMsgCode) + "[" +object.getId() + " : " +object.getName() + "]");
				throw new EIMApplicationException(WebDAVStatus.SC_FORBIDDEN);
			}

			/* ------------------------------------------------------------------
			 * ここからWeDAVのロック可能判定を行う
			 * -----------------------------------------------------------------*/
			// ロック状況の確認
			if(isLocked(object)) {
				//ロックされていた場合、Exceptionをスロー
				log.error(ResourceUtils.getByKey("EIM.ERROR.LOGIC.OBJECT.LOCKED") + "[" + object.getId() + " : " +object.getName() + "]");
				throw new EIMApplicationException(WebDAVStatus.SC_LOCKED);
			}

			LockInfo lock = null;
			lock = new LockInfo();
			//lockRequestTypeの設定
			int lockRequestType = LOCK_CREATION;
			Node lockInfoNode = null;
			DocumentBuilder documentBuilder = webDAVObjectDomain.getDocumentBuilder();
			try {
				// 実行時、「[Fatal Error] :-1:-1: Premature end of file.」が出力されるが、
				// DocumentBuilderの内部で出力しているので制御できない
				Document document = documentBuilder.parse(new InputSource(webDAVObjectDomain.getServletInputStream()));
				Element rootElement = document.getDocumentElement();
				lockInfoNode = rootElement;
			} catch(Exception e) {
				lockRequestType = LOCK_REFRESH;
			}

			//LockInfo設定
			lock = setLockInfo(webDAVObjectDomain, lock, lockInfoNode);
			//LockToken生成
			String lockToken = createLockToken(webDAVObjectDomain, lock);

			if(lockRequestType == LOCK_CREATION) {
				// ロックする
				// LockInfoはそのまま利用する
				lock.tokens.add(lockToken);
			}
			if(lockRequestType == LOCK_REFRESH) {
				// ロック期限を延ばす
				// 本来ならロック作成時のトークンを返却すれば良いのだが、
				// 作成し直しても同じ動作をするので、作成したトークンを返却する
				lock.tokens.add(lockToken);
			}

			// WebDAVロックフラグを追加する。ロックユーザ、ロック日時を追加する。

			// ロックをかける
			EIMSession sess = jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.getEIMSession();
			EIMObject eimObject = ObjectUtils.getObjectById(sess, object.getId());
			ObjectUtils.lock(sess, eimObject);

			// WebDAVロックフラグをセット
			EIMAttributeType attributeType = AttributeUtils.getAttributeTypeByName(sess, ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_WEBDAV_LOCK_FLAG"));
			ObjectAttributeUtils.setAttribute(sess, eimObject, attributeType, 1);


			XMLWriter generatedXML = new XMLWriter();
			generatedXML.writeXMLHeader();
			generatedXML.writeElement(null, "prop" + generateNamespaceDeclarations(), XMLWriter.OPENING);
			generatedXML.writeElement(null, "lockdiscovery", XMLWriter.OPENING);
			lock.toXML(generatedXML);
			generatedXML.writeElement(null, "lockdiscovery", XMLWriter.CLOSING);
			generatedXML.writeElement(null, "prop", XMLWriter.CLOSING);

			//ステータスコード設定
			webDAVObjectDomain.setStatusCode(WebDAVStatus.SC_OK);

			Writer writer = webDAVObjectDomain.getWriter();
			writer.write(generatedXML.toString());
			writer.close();
		}
	}

	/**
	 * @see jp.co.ctc_g.eim.framework2.business.service.WebDAVService#doUnlock(jp.co.ctc_g.eim.framework2.business.domain.entity.WebDAVObjectDomain)
	 * @since Ver6.0
	 */
	@Override
	public void doUnlock(WebDAVObjectDomain webDAVObjectDomain) throws EIMApplicationException, Exception{

		if(!isDocument(webDAVObjectDomain.getId())) {

			// ドキュメント以外の場合、親クラスの処理のみ実行
			super.doUnlock(webDAVObjectDomain);

		} else {

			// DBロックをかける
			ObjectDomain object = getObjectService().getByIdForUpdate(webDAVObjectDomain.getId());

			// ロック状況の確認
			if(isLocked(object)) {
				//ロックされていた場合、Exceptionをスロー
				log.error(ResourceUtils.getByKey("EIM.ERROR.LOGIC.OBJECT.LOCKED") + "[" + object.getId() + " : " +object.getName() + "]");
				throw new EIMApplicationException(WebDAVStatus.SC_LOCKED);
			}

			// ロックを解除する
			EIMSession sess = jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.getEIMSession();
			EIMObject eimObject = ObjectUtils.getObjectById(sess, object.getId());
			ObjectUtils.unLock(sess, eimObject);

			// WebDAVロックを解除する
			EIMAttributeType attributeType = AttributeUtils.getAttributeTypeByName(sess, ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_WEBDAV_LOCK_FLAG"));
			ObjectAttributeUtils.deleteAttribute(sess, eimObject, attributeType);

			//ステータスコード設定
			webDAVObjectDomain.setStatusCode(WebDAVStatus.SC_NO_CONTENT);
		}

		// SearchFramework 検索FW更新通知 対象：ドキュメント
		AppUpdateNoticeUtils.updateNoticeInsert(webDAVObjectDomain.getId(), "SEARCHFW_CHECK_IN_DOCUMENT");
	}

	public File getOriginalFile(ObjectDomain object, FormatDomain format) throws EIMApplicationException, Exception {

		// 対象ファイルのメタ情報を取得
		FileDomain file = fileDao.getByObjectAndFormat(object, format);
		if(file == null) {
			// 該当する{0}ファイルオブジェクトが存在しません。[{1}]
			log.error(ResourceUtils.getByKey("EIM.ERROR.LOGIC.FAILED.OCR.PROCESS.NO.FILEOBJECT",new Object[]{format.getName(),object.getName()}));
			throw new EIMApplicationException(WebDAVStatus.SC_FORBIDDEN);
		}

		// オンラインディレクトリを指定
		file.setDirectory(format.getOnlineDirectory());

		// 実ファイルを取得する
		File originalFile = new File(FileUtil.getFilePath(object, file));
		if(!originalFile.exists()) {
			// 該当する{0}ファイルオブジェクトが存在しません。[{1}]
			log.error(ResourceUtils.getByKey("EIM.ERROR.LOGIC.FAILED.OCR.PROCESS.NO.FILEOBJECT",new Object[]{format.getName(),object.getName()}));
			throw new EIMApplicationException(WebDAVStatus.SC_FORBIDDEN);
		}

		return originalFile;
	}

	public File getPublicFile(ObjectDomain object, FormatDomain format) throws EIMApplicationException, Exception {

		// 対象ファイルのメタ情報を取得
		FileDomain file = fileDao.getByObjectAndFormat(object, format);

		// オンラインディレクトリを指定
		file.setDirectory(format.getOnlineDirectory());

		// 実ファイルを取得する
		File publicFile = new File(FileUtil.getFilePath(object, file));

		return publicFile;
	}


	private boolean isLocked(ObjectDomain object) throws Exception{
		// ログインユーザ
		TransactionContext tx = EIMThreadContext.getTransactionContext();
		UserDomain loginUser = tx.getUser();

		//
		AttributeDomain attribute = object.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_WEBDAV_LOCK_FLAG"));
		if (attribute == null) {
			// WebDAVロックフラグがnullの場合はロックされていないものとする
			return false;
		}

		// ロックユーザ、ロック日時が設定されている場合はロックされているとみなす
		if (object.getLockUser() != null || object.getLockDate() != null) {
			if (object.getLockUser().getId() != loginUser.getId()) {

				// ロックユーザとログインユーザが異なる場合はロックされているとみなす
				// 期限切れの場合はロックされていないものとみなす
				if (isExpired(object)) {
					return false;
				} else {
					return true;
				}

			} else {
				// ロックユーザとログインユーザが同じ場合はロックされていないものとみなす
				return false;
			}
		}

		return false;
	}

	/**
	 * ロック期限切れかどうかを判定する
	 * @param object
	 * @return
	 * @throws Exception
	 */
	private boolean isExpired(ObjectDomain object) throws Exception{

		// 更新日時
		Date updateDate = object.getModificationDate();

		if (updateDate == null) {
			return false;
		}

		long lockDateMSec = updateDate.getTime();
		// 更新日時にWebDAVロック期間をミリ秒に変換して加算する
		long expireAtMSec = lockDateMSec + (defaultTimeout * 1000);

		// システム日時
		long currentDateMSec = System.currentTimeMillis();

		System.out.println("lockDateMSec    = " + (new Date(lockDateMSec)));
		System.out.println("expireAtMSec    = " + (new Date(expireAtMSec)));
		System.out.println("currentDateMSec = " + (new Date(currentDateMSec)));

		if (expireAtMSec < currentDateMSec) {
			// ロック期限切れ
			return true;
		}

		// ロック期限切れではない
		return false;
	}

	/**
	 * Lock情報を設定します。<br>
	 * <br>
	 * @param webDAVObjectDomain WebDAV通信用オブジェクト
	 * @param lock ロック情報
	 * @param lockInfoNode ロック情報ノード
	 * @return ロック情報
	 * @since Ver6.0
	 */
	private LockInfo setLockInfo(WebDAVObjectDomain webDAVObjectDomain, LockInfo lock, Node lockInfoNode) throws Exception {
		//expiresAtの設定
		int lockDuration = defaultTimeout;
		String lockDurationStr = webDAVObjectDomain.getTimeoutHeader();
		if(lockDurationStr == null) {
			lockDuration = defaultTimeout;
		} else {
			int commaPos = lockDurationStr.indexOf(",");
			if(commaPos != -1) {
				lockDurationStr = lockDurationStr.substring(0,commaPos);
			}
			if(lockDurationStr.startsWith("Second-")) {
				lockDuration = (new Integer(lockDurationStr.substring(7))).intValue();
			} else {
				if(lockDurationStr.equalsIgnoreCase("infinity")) {
					lockDuration = MAX_TIMEOUT;
				} else {
					try {
						lockDuration = (new Integer(lockDurationStr)).intValue();
					} catch (NumberFormatException e) {
						lockDuration = MAX_TIMEOUT;
					}
				}
			}
			if(lockDuration == 0) {
				lockDuration = defaultTimeout;
			}
			if(lockDuration > MAX_TIMEOUT) {
				lockDuration = MAX_TIMEOUT;
			}
		}
		lock.expiresAt = System.currentTimeMillis() + (lockDuration * 1000);

		//lockInfoNode生成
		//scope、type、ownerの設定
		if(lockInfoNode != null) {
			NodeList childList = lockInfoNode.getChildNodes();
			StringWriter strWriter = null;
			DOMWriter domWriter = null;
			Node lockScopeNode = null;
			Node lockTypeNode = null;
			Node lockOwnerNode = null;

			for(int i=0; i < childList.getLength(); i++) {
				Node currentNode = childList.item(i);
				switch(currentNode.getNodeType()) {
				case Node.TEXT_NODE:
					break;
				case Node.ELEMENT_NODE:
					String nodeName = currentNode.getNodeName();
					if(nodeName.endsWith("lockscope")) lockScopeNode = currentNode;
					if(nodeName.endsWith("locktype")) lockTypeNode = currentNode;
					if(nodeName.endsWith("owner")) lockOwnerNode = currentNode;
					break;
				}
			}

			if(lockScopeNode != null) {
				childList = lockScopeNode.getChildNodes();
				for(int i=0; i < childList.getLength(); i++) {
					Node currentNode = childList.item(i);
					switch(currentNode.getNodeType()) {
					case Node.TEXT_NODE:
						break;
					case Node.ELEMENT_NODE:
						String tempScope = currentNode.getNodeName();
						if(tempScope.indexOf(':') != -1) {
							lock.scope = tempScope.substring(tempScope.indexOf(':') + 1);
						} else {
							lock.scope = tempScope;
						}
						break;
					}
				}
				if(lock.scope == null) {
					webDAVObjectDomain.setStatusCode(WebDAVStatus.SC_BAD_REQUEST);
				}
			} else {
				webDAVObjectDomain.setStatusCode(WebDAVStatus.SC_BAD_REQUEST);
			}

			if(lockTypeNode != null) {
				childList = lockTypeNode.getChildNodes();
				for(int i=0; i < childList.getLength(); i++) {
					Node currentNode = childList.item(i);
					switch(currentNode.getNodeType()) {
					case Node.TEXT_NODE:
						break;
					case Node.ELEMENT_NODE:
						String tempType = currentNode.getNodeName();
						if(tempType.indexOf(':') != -1) {
							lock.type = tempType.substring(tempType.indexOf(':') + 1);
						} else {
							lock.type = tempType;
						}
						break;
					}
				}
				if(lock.type == null) {
					webDAVObjectDomain.setStatusCode(WebDAVStatus.SC_BAD_REQUEST);
				}
			} else {
				webDAVObjectDomain.setStatusCode(WebDAVStatus.SC_BAD_REQUEST);
			}

			if(lockOwnerNode != null) {
				childList = lockOwnerNode.getChildNodes();
				for(int i=0; i < childList.getLength(); i++) {
					Node currentNode = childList.item(i);
					switch(currentNode.getNodeType()) {
					case Node.TEXT_NODE:
					case Node.ELEMENT_NODE:
						TransactionContext tran = EIMThreadContext.getTransactionContext();
						UserDomain userDomain = tran.getUser();
						lock.owner+=userDomain.getCode();
						break;
					}
				}
				if(lock.owner == null) {
					webDAVObjectDomain.setStatusCode(WebDAVStatus.SC_BAD_REQUEST);
				}
			} else {
				lock.owner = new String();
			}
		}

		return lock;
	}

	/**
	 * ロック情報からLockTokenを生成します。<br>
	 * <br>
	 * @param webDAVObjectDomain WebDAV通信用オブジェクト
	 * @param lock ロック情報
	 * @return LockToken
	 * @since Ver6.0
	 */
	private String createLockToken(WebDAVObjectDomain webDAVObjectDomain, LockInfo lock) throws Exception {

		//LockToken生成
		String lockTokenStr = webDAVObjectDomain.getServletPath() + webDAVObjectDomain.getId()
		+ "-"
		+ lock.type
		+ "-"
		+ lock.scope
		+ "-"
		+ webDAVObjectDomain.getUserPrincipal()
		+ "-"
		+ lock.depth
		+ "-"
		+ lock.owner
		+ "-"
		+ lock.tokens
		+ "-"
		+ lock.expiresAt
		+ "-"
		+ System.currentTimeMillis()
		+ "-"
		+ webDAVObjectDomain.getSecret();

		//LockTokenの暗号化
		try {
			md5Helper = MessageDigest.getInstance("MD5");
		} catch (NoSuchAlgorithmException e) {
			log.error(e.getMessage());
			log.error(e.getStackTrace());
			throw new UnavailableException("No MD5");
		}
		String lockToken = md5encode(md5Helper.digest(lockTokenStr.getBytes()));

		return lockToken;
	}

	/**
	 * ロック情報保持クラス
	 */
	private class LockInfo {

		String type = "write";
		String scope = "exclusive";
		int depth = 0;
		String owner = "";
		List<Object> tokens = new Vector<Object>();
		long expiresAt = 0;

		/**
		 * コンストラクタ
		 */
		public LockInfo() {}

		/**
		 * lockTokenを文字列型で取得する。
		 */
		public String toString() {
			String result = "Type:" + type + "\n";
			result += "Scope:" + scope + "\n";
			result += "Depth:" + depth + "\n";
			result += "Owner:" + owner + "\n";
			result += "Expiration:" + FastHttpDateFormat.formatDate(expiresAt, null) + "\n";

			Iterator<Object> iter = tokens.iterator();
			while(iter.hasNext()) {
				result += "Token:" + iter.next() + "\n";
			}

			return result;
		}

		/**
		 * LockTokenをXML形式に変換
		 */
		public void toXML(XMLWriter generatedXML) {
			generatedXML.writeElement(null, "activelock", XMLWriter.OPENING);
			generatedXML.writeElement(null, "locktype", XMLWriter.OPENING);
			generatedXML.writeElement(null, type, XMLWriter.NO_CONTENT);
			generatedXML.writeElement(null, "locktype", XMLWriter.CLOSING);
			generatedXML.writeElement(null, "lockscope", XMLWriter.OPENING);
			generatedXML.writeElement(null, scope, XMLWriter.NO_CONTENT);
			generatedXML.writeElement(null, "lockscope", XMLWriter.CLOSING);
			generatedXML.writeElement(null, "depth", XMLWriter.OPENING);
			if(depth == INFINITY)
				generatedXML.writeText("Infinity");
			else
				generatedXML.writeText("0");
			generatedXML.writeElement(null, "depth", XMLWriter.CLOSING);
			generatedXML.writeElement(null, "owner", XMLWriter.OPENING);
			generatedXML.writeText(owner);
			generatedXML.writeElement(null, "owner", XMLWriter.CLOSING);
			generatedXML.writeElement(null, "timeout", XMLWriter.OPENING);
			long timeout = (expiresAt - System.currentTimeMillis()) / 1000;
			generatedXML.writeText("Second-" + timeout);
			generatedXML.writeElement(null, "timeout", XMLWriter.CLOSING);
			generatedXML.writeElement(null, "locktoken", XMLWriter.OPENING);
			Iterator<Object> iter = tokens.iterator();
			while (iter.hasNext()) {
				generatedXML.writeElement(null, "href", XMLWriter.OPENING);
				generatedXML.writeText("opaquelocktoken:" + iter.next());
				generatedXML.writeElement(null, "href", XMLWriter.CLOSING);
			}
			generatedXML.writeElement(null, "locktoken", XMLWriter.CLOSING);
			generatedXML.writeElement(null, "activelock", XMLWriter.CLOSING);
		}
	}

	/**
	 * ネームスペース生成
	 */
	private String generateNamespaceDeclarations() throws Exception{
		return " xmlns=\"" + DEFAULT_NAMESPACE + "\"";
	}

	/**
	 * ドキュメント判定の処理。<br>
	 * <br>
	 * @param id オブジェクトID<br>
	 * @return ドキュメントの場合、true<br>
	 * <br>
	 */
	private boolean isDocument(long id) throws Exception {

		// オブジェクトタイプ定義名称
		ObjectDomain obj = super.getObjectService().getById(id);
		String objTypeDefName = obj.getType().getDefinitionName();

		if(objTypeDefName.equals(ConfigUtils.getByKey("OBJECT_TYPE_NAME_TEMP_ATTACH_FILE"))) {
			// オブジェクトタイプが一時添付ファイルの場合、false
			return false;
		} else {
			// オブジェクトタイプが一時添付ファイル以外(=ドキュメント)の場合、true
			return true;
		}
	}

	/**
	 * 対象ドキュメントのロック状態判定処理。<br>
	 * 以下のチェックを行う<br>
	 * ＜ステータスが承認依頼中の場合＞<br>
	 *   対象ドキュメントのluserの有無<br>
	 * ＜ステータスが承認依頼中以外の場合＞<br>
	 *   ロックオブジェクト作成ユーザとログインユーザの一致<br>
	 * <br>
	 * @param object ドキュメントオブジェクト<br>
	 * @return チェック結果、エラー時メッセージコードを返却<br>
	 * <br>
	 */
	private String checkLockInfo(ObjectDomain object) throws Exception {

		// EIMSession
		EIMSession sess = jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.getEIMSession();

		// EIMVER取得
		// ObjectDomainをEIMObjectに変換(バージョン検索に必要なものはobjIdのみ)
		EIMObject obj = new EIMObject(
							object.getId()
							, null
							, object.getName()
							, object.getRevision()
							, object.isLatest()
							, null
							, object.getCreationDate()
							, null
							, object.getModificationDate()
							, null
							, object.getLockDate()
							, false
							, true
							, null
						);
		EIMVersion version = VersionUtils.getVersion(sess, obj);

		// 最新のリビジョンかをチェック
		if (object.getRevision() != version.getMaxRevision()) {
			// 最新でない場合
			return "EIM.ERROR.LOGIC.NOCHECKOUTUSER";
		}

		// ロックユーザをチェック
		// ※チェックアウト無しでチェックインする場合は、過去ドキュメントはロックされていない
		if (object.getRevision() > 0) {
			// トランザクションからユーザを取得
			TransactionContext tx = EIMThreadContext.getTransactionContext();
			UserDomain loginUser = tx.getUser();

			long statusTypeKindId = AppConstant.STATUS_TYPE_KIND_ID_NONE;
			// WFありの場合
			if (object.getStatus() != null) {
				// ステータス取得
				statusTypeKindId = object.getStatus().getType().getBase().getId();
			}
			// 「承認依頼中」の場合
			if (statusTypeKindId == AppConstant.STATUS_TYPE_KIND_ID_APPROVE) {
				// オブジェクトのlockuserの有無、ロック者がログインユーザかどうかでロック状態を判断
				// 承認依頼中の場合、設定によりチェックイン可能なため
				// ※ユーザ・ステータス自体がチェックイン可能かはステータスチェックで行う
				if (object.getLockUser() != null && object.getLockUser().getId() != loginUser.getId()) {
					return "EIM.ERROR.LOGIC.OBJECT.LOCKED";
				}

			// 「承認依頼中」以外、またはWFなしの場合
			} else {

				// ロックオブジェクト取得
				EIMObject lockObj = version.getObjectByRev(object.getRevision() - 1);
				// オブジェクトがロックされている場合のみ、ユーザーの比較を行う
				if (lockObj != null && lockObj.getLockUser() != null && lockObj.getLockUser().getId() != loginUser.getId()) {
					// ロックユーザとログインユーザが異なる場合
					return "EIM.ERROR.LOGIC.NOCHECKOUTUSER";
				}
			}
		}

		return "";
	}

	/**
	 * 対象ドキュメントが編集できる状態か判定の処理。<br>
	 *  ・ステータスを判定
	 *  ・権限判定
	 *  ・ロックユーザ判定
	 * <br>
	 * @param objectId ドキュメントオブジェクトID<br>
	 * <br>
	 */
	public void checkStatus(long objectId) throws Exception {

		// 対象オブジェクト取得
		ObjectDomain object = super.getObjectService().getById(objectId);

		// 書き込み権限判定
		TransactionContext tx = EIMThreadContext.getTransactionContext();
		UserDomain loginUser = tx.getUser();
		if (!getObjectService().authorized(loginUser, object, new AccessRoleTypeDomain(EIMAccessRole.UPDATE))) {
			// チェックイン権限がありません
			throw new EIMApplicationException("EIM.ERROR.LOGIC.NOCHECKINROLE");
		}

		// ステータスチェック実行
		String errMsgCode = checkStatusExec(object);
		if (!org.apache.commons.lang3.StringUtils.isEmpty(errMsgCode)) {
			Object[] msgParamArr = null;
			if (MSG_CODE_DISABLE_APPROVER_CHECKIN.equals(errMsgCode)
				|| MSG_CODE_CANNOT_CHECKIN_ONLY_APPROVER.equals(errMsgCode)) {
				msgParamArr = new Object[]{object.getName(), ResourceUtils.getByKey("EIM.ACCESS.TYPE.EDIT")};
			} else {
				msgParamArr = new Object[]{object.getName()};
			}
			// errMsgCodeが空の場合エラーとする
			throw new EIMApplicationException(errMsgCode, msgParamArr);
		}

		// ロックチェック(エラー時、メッセージコードが返却される)
		errMsgCode = checkLockInfo(object);
		if (!org.apache.commons.lang3.StringUtils.isEmpty(errMsgCode)) {
			// チェックアウトを実行したユーザではありません。
			throw new EIMApplicationException(errMsgCode);
		}
	}

	/**
	 * 対象ドキュメントが編集できるステータスか判定の処理。<br>
	 * <br>
	 * @param object ドキュメントオブジェクト<br>
	 * @return 編集できる場合は空文字、編集できない場合はエラーメッセージ<br>
	 * <br>
	 */
	private String checkStatusExec(ObjectDomain object) throws Exception {
		String errMsgCode = "";

		//ステータスのチェック
		// 承認中チェックイン許可オプションがOFFの場合
		if (OptionConfData.getInstance().enableApproverCheckin == false) {
			// ステータス ワークフロー有りの場合は編集中のみ可能
			if (object.getStatus() != null) {
				if (!(object.getStatus().getType().getBase().getId() == AppConstant.STATUS_TYPE_KIND_ID_EDITTING)) {
					errMsgCode = "EIM.ERROR.LOGIC.NOTUPDATING";
				}
			}
		}

		// 承認中チェックイン許可オプションがONの場合
		if (OptionConfData.getInstance().enableApproverCheckin == true) {
			// WFありの場合
			if (object.getStatus() != null) {
				// ステータスが「編集中」「承認依頼中」以外の場合、エラー
				long statusTypeKindId = object.getStatus().getType().getBase().getId();
				if (statusTypeKindId != AppConstant.STATUS_TYPE_KIND_ID_EDITTING
					&& statusTypeKindId != AppConstant.STATUS_TYPE_KIND_ID_APPROVE) {

					errMsgCode = "EIM.ERROR.LOGIC.NOTUPDATING";

				// ステータスが「承認依頼中」の場合
				} else if (statusTypeKindId == AppConstant.STATUS_TYPE_KIND_ID_APPROVE) {
					WorkflowDomain workflow = workflowService.getByObject(object);

					// WFがない場合、エラー
					if (workflow == null) {
						errMsgCode = "EIM.ERROR.LOGIC.WORKFLOW.NOT.APPLIED";

					} else {
						//ワークフロー設定オブジェクト取得
						ObjectTypeDomain workflowSettingObjType = objectTypeService.getByDefinitionName(ConfigUtils.getByKey("OBJECT_TYPE_NAME_WFSETTING"));
						ObjectDomain workflowSettingObj = super.getObjectService().getByTypeAndName(workflowSettingObjType, String.valueOf(workflow.getId()));

						// 「チェックイン可能ステータス」属性を取得し、現在のステータスがチェックインを許可しているかをチェック
						boolean enableCheckStatus = false;
						AttributeDomain enableCheckinStatusAttr = workflowSettingObj.getAttribute(ConfigUtils.getByKey("ATTR_NAME_ENABLE_CHECKIN_STATUS"));
						if (enableCheckinStatusAttr != null) {
							List<Long> enableCheckinStatusList = enableCheckinStatusAttr.getLongList();
							for (long enableCheckinStatus : enableCheckinStatusList) {
								if (object.getStatus().getType().getId() == enableCheckinStatus) {
									enableCheckStatus = true;
								}
							}
						}
						// 現在のステータスがチェックインを許可していない場合、エラー
						if (!enableCheckStatus) {
							errMsgCode = MSG_CODE_DISABLE_APPROVER_CHECKIN;
						}
					}

					if (org.apache.commons.lang3.StringUtils.isEmpty(errMsgCode)) {
						TransactionContext tx = EIMThreadContext.getTransactionContext();
						UserDomain loginUser = tx.getUser();

						// ログインユーザが現ステータスのエントリに入っているかチェック
						if (!AppWorkFlowUtil.isUserEntriedApproverCheck(object.getStatus().getId(), loginUser.getId())) {
							errMsgCode = MSG_CODE_CANNOT_CHECKIN_ONLY_APPROVER;
						}
					}
				}
			// WFなしの場合
			} else {
				// 何もしない(ドキュメントがロックされているかは、その後の処理でチェックしているため)
				return errMsgCode;
			}
		}
		return errMsgCode;
	}

	/**
	 * 属性タイプサービスを取得します。<br>
	 * <br>
	 * @return 属性タイプサービス
	 * @since Ver6.0
	 */
	public AttributeTypeService getAttributeTypeService() {
		return attributeTypeService;
	}

	/**
	 * 属性タイプサービスを設定します。<br>
	 * @param attributeTypeService 属性タイプサービス
	 * @since Ver6.0
	 */
	public void setAttributeTypeService(AttributeTypeService attributeTypeService) {
		this.attributeTypeService = attributeTypeService;
	}

	/**
	 * オブジェクトタイプサービスを取得します。<br>
	 * <br>
	 * @return オブジェクトタイプサービス
	 * @since Ver6.0
	 */
	public ObjectTypeService getObjectTypeService() {
		return objectTypeService;
	}

	/**
	 * オブジェクトタイプサービスを設定します。<br>
	 * @param objectTypeService オブジェクトタイプサービス
	 * @since Ver6.0
	 */
	public void setObjectTypeService(ObjectTypeService objectTypeService) {
		this.objectTypeService = objectTypeService;
	}

	/**
	 * ファイルDaoを取得します。
	 * @return ファイルDao
	 */
	public FileDao getFileDao() {
		return fileDao;
	}

	/**
	 * ファイルDaoを設定します。
	 * @param fileDao ファイルDao
	 */
	public void setFileDao(FileDao fileDao) {
		this.fileDao = fileDao;
	}

	/**
	 * ワークフローサービスを取得します。<br>
	 * <br>
	 * @return ワークフローサービス
	 * @since Ver6.7
	 */
	public WorkflowService getWorkflowService() {
		return workflowService;
	}

	/**
	 * ワークフローサービスを設定します。<br>
	 * @param workFlowService ワークフローサービス
	 * @since Ver6.7
	 */
	public void setWorkflowService(WorkflowService workflowService) {
		this.workflowService = workflowService;
	}

}
