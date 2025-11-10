package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;

import common.util.AppConstant;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectTypeUtil;
import common.util.AppObjectUtil;
import common.util.AppUpdateNoticeUtils;
import common.util.DisplayColorUtil;
import common.util.OptionConfData;
import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMDirectory;
import eim.bo.EIMFile;
import eim.bo.EIMFormat;
import eim.bo.EIMGroup;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMRelationType;
import eim.bo.EIMResource;
import eim.bo.EIMSecurity;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.AccessUtils;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.FileUtils;
import eim.util.GroupUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.OperationHistoryUtils;
import eim.util.RelationUtils;
import eim.util.SecurityUtils;
import eim.util.TypeConvertUtils;
import eim.util.UserUtils;
import eim.util.WorkFlowUtils;
import jp.co.ctc_g.eim.app.document.business.service.CoverDocumentFormService;
import jp.co.ctc_g.eim.app.document.business.service.PDFService;
import jp.co.ctc_g.eim.app.document.business.service.QRCodeService;
import jp.co.ctc_g.eim.app.form.business.domain.FormDomain;
import jp.co.ctc_g.eim.framework.business.service.FileIoService;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AccessRoleTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.OtherNameDomain;
import jp.co.ctc_g.eim.framework2.common.exception.EIMApplicationException;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;
import jp.co.ctc_g.eim.framework2.common.util.ResourceUtils;

public class CoverDocumentFormServiceImpl extends DocumentFormServiceImpl implements CoverDocumentFormService {
	/** ファイルIOサービス */
	private FileIoService fileIoService;
	/** QRコードサービス */
	private QRCodeService qrCodeService;
	/** PDFサービス */
	private PDFService pdfService;
	/** 日付フォーマット：YYYY/MM/DD */
	private static final String CREATION_DATE_FORMAT = "yyyy/MM/dd";

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.CoverDocumentFormService#createCover(jp.co.ctc_g.eim.app.form.business.domain.FormDomain)
	 * @since Ver 6.6
	 */
	public FormDomain createCover(FormDomain form) throws Exception {

		EIMSession sess = EIMThreadContext.getEIMSession();
		File pdfFile = null;
		try {
			//---------------------------------------------
			// 設定取得
			//---------------------------------------------
			// QRコードサイズ
			int qrCodeScale = Integer.parseInt(ConfigUtils.getByKey("COVER_DOC_QR_CODE_SCALE"));
			// QRコード出力位置_X軸
			int qrCodePosX = Integer.parseInt(ConfigUtils.getByKey("COVER_DOC_QR_CODE_POS_X"));
			// QRコード出力位置_Y軸
			int qrCodePosY = Integer.parseInt(ConfigUtils.getByKey("COVER_DOC_QR_CODE_POS_Y"));
			// 表紙出力情報フォントサイズ
			int coverAttrFontSize = Integer.parseInt(ConfigUtils.getByKey("COVER_DOC_ATTR_FONT_SIZE"));
			// 表紙出力属性行間隔
			int coverAttrLineHeightLarge = Integer.parseInt(ConfigUtils.getByKey("COVER_DOC_ATTR_LINE_HEIGHT_LARGE"));
			// 表紙出力属性行間隔
			int coverAttrLineHeight = Integer.parseInt(ConfigUtils.getByKey("COVER_DOC_ATTR_LINE_HEIGHT"));
			// 表紙出力作成日出力位置_X軸
			float coverCdatePosX = Float.parseFloat(ConfigUtils.getByKey("COVER_DOC_CDATE_POS_X"));
			// 表紙出力作成日出力位置_Y軸
			float coverCdatePosY = Float.parseFloat(ConfigUtils.getByKey("COVER_DOC_CDATE_POS_Y"));
			// 表紙出力属性出力位置_X軸
			float coverAttrPosX = Float.parseFloat(ConfigUtils.getByKey("COVER_DOC_ATTR_POS_X"));
			// 表紙出力属性出力位置_Y軸
			float coverAttrPosY = Float.parseFloat(ConfigUtils.getByKey("COVER_DOC_ATTR_POS_Y"));

			// スキャン用表紙作成可能かをチェックする
			if (!checkCoverMakeAuth()) {
				throw new EIMApplicationException("EIM.ERROR.LOGIC.NOROLE");
			}

			// 親オブジェクト(WS / フォルダ)取得
			Long parentId = null;
			for (AttributeDomain attribute : form.getAttributeList()) {
				// 属性「帳票タイプフォルダ」の場合、親オブジェクトIDを設定
				if (attribute.getAttributeType().getDefinitionName().equals(ConfigUtils.getByKey("ATTRIBUTE_TYPE_NAME_FORM_TYPE_FOLDER_ID"))) {
					parentId = attribute.getLong();
					break;
				}
			}
			FormDomain parentForm = super.getById(parentId);

			// 親オブジェクトの存在チェック
			EIMObject parentObj = ObjectUtils.getObjectById(sess, parentForm.getId());
			if (AppObjectUtil.isObjectInRecycle(sess, parentObj)) {
				throw new EIMApplicationException("EIM.ERROR.LOGIC.SELECTFOLDERDELETED");
			}

			// ドキュメント名に"[表紙]-"を付加する
			form.setName(ConfigUtils.getByKey("PDF_AUTO_REGIST_DOC_NAME_PREFIX") + form.getName());

			// スキャン用表紙ドキュメント登録
			FormDomain coverDocument = createWithoutFile(form, parentForm);

			// 属性：スキャン用表紙フラグをセット
			EIMObject coverObj = new EIMObject(coverDocument.getId(), null, null, 0, false, null, null, null, null, null, null, false, false, null);
			AppObjectUtil.setAttr(sess, coverObj, ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_COVER_FOR_SCAN"), AppConstant.FLAG_ON);

			// スキャン用表紙ドキュメントにチェックインする原本フォーマットのPDFを作成

			// 表紙PDF生成
			String langId = (String)EIMThreadContext.getTransactionContext().getLangId();
			File workDir = new File(ConfigUtils.getByKey("PDF_AUTO_REGIST_WORK"));
			pdfFile = File.createTempFile("eimCover_", ConfigUtils.getByKey("PDF_EXT"), workDir);
			List<String> outputAttrInfoList = new ArrayList<String>();

			// ドキュメントタイプ名取得
			String documentTypeName = "";
			if (form.getType().getDefinitionName().equals(EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT"))) {
				// 「ドキュメント」の場合は「一般ドキュメント」に変換する
				documentTypeName = EIMResource.getMessage(sess, "EIM.OBJECTTYPE.GENERAL");
			} else {
				for (OtherNameDomain otherName : form.getType().getNameList()) {
					if (otherName.getLangId().equals(langId)) {
						documentTypeName = otherName.getName();
					}
				}
			}

			// 作成者名取得
			long userId = coverDocument.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_CREATE")).getLong();
			EIMUser createUser = UserUtils.getUserById(sess, userId);
			String createUserName = createUser.getName();
			DateFormat sdf = new SimpleDateFormat(CREATION_DATE_FORMAT);
			String createDateStr = sdf.format(new Date());

			// 表紙PDFに出力するシステム属性を設定
			// 固定値：「スキャン用表紙」
			outputAttrInfoList.add(ResourceUtils.getByKeyWithLang(langId, "EIM.E.DOCUMENT.FILEINFO"));
			// パス
			outputAttrInfoList.add(ResourceUtils.getByKeyWithLang(langId, "EIM.E.DOCUMENT.PATH", coverDocument.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PASS")).getString()));
			// ファイル名
			outputAttrInfoList.add(ResourceUtils.getByKeyWithLang(langId, "EIM.E.DOCUMENT.FILENAME", form.getName()));
			// ドキュメントタイプ
			outputAttrInfoList.add(ResourceUtils.getByKeyWithLang(langId, "EIM.E.DOCUMENT.TYPE", documentTypeName));
			// 作成者
			outputAttrInfoList.add(ResourceUtils.getByKeyWithLang(langId, "EIM.E.DOCUMENT.CREATIONUSER", createUserName));
			// 作成日
			outputAttrInfoList.add(ResourceUtils.getByKeyWithLang(langId, "EIM.E.DOCUMENT.CREATIONDATE", createDateStr));

			// 表紙PDF作成
			pdfService.make(
				ConfigUtils.getByKey("PDF_COVER_FONT_FILE")
				, pdfFile.getAbsolutePath()
				, outputAttrInfoList
				, coverAttrFontSize
				, coverCdatePosX
				, coverCdatePosY
				, coverAttrPosX
				, coverAttrPosY
				, coverAttrLineHeightLarge
				, coverAttrLineHeight
				, langId
			);

			// 表紙PDFにQRコード出力
			String lpadId = StringUtils.leftPad(String.valueOf(coverDocument.getId()), AppConstant.QR_CODE_OBJID_MAX_SCALE, "0");
			BufferedImage qrCodeImage = qrCodeService.create(AppConstant.QR_CODE_EMBEDDED_HEADER + lpadId, AppConstant.QR_CODE_IMAGE_SIZE);
			pdfService.putImage(pdfFile, qrCodeImage, qrCodeScale, qrCodePosX, qrCodePosY);

			// 作成したPDFをドキュメントオブジェクトにチェックイン
			checkInFile(coverDocument, parentForm, pdfFile);

			return coverDocument;

		} finally {
			if (pdfFile != null) {
				pdfFile.delete();
			}
		}
	}
	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.CoverDocumentFormService#createWithoutFile(jp.co.ctc_g.eim.app.form.business.domain.FormDomain, jp.co.ctc_g.eim.app.form.business.domain.FormDomain)
	 * @since Ver6.6
	 */
	public FormDomain createWithoutFile(FormDomain form, FormDomain parentForm) throws Exception, EIMApplicationException {
		return createWithoutFile(form, parentForm, true);
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.CoverDocumentFormService#createWithoutFileForce(jp.co.ctc_g.eim.app.form.business.domain.FormDomain, jp.co.ctc_g.eim.app.form.business.domain.FormDomain)
	 * @since Ver6.6
	 */
	public FormDomain createWithoutFileForce(FormDomain form, FormDomain parentForm) throws Exception, EIMApplicationException {
		return createWithoutFile(form, parentForm, false);
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.CoverDocumentFormService#checkInFile(jp.co.ctc_g.eim.app.form.business.domain.FormDomain, jp.co.ctc_g.eim.app.form.business.domain.FormDomain, java.io.File)
	 * @since Ver6.6
	 */
	public void checkInFile(FormDomain documentForm, FormDomain parentForm, File targetFile) throws Exception {
		FileInputStream fis = null;
		try {
			EIMSession sess = EIMThreadContext.getEIMSession();

			EIMObject documentObj = new EIMObject(documentForm.getId(), null, null, 0, false, null, null, null, null, null, null, false, false, null);
			ObjectTypeDomain docObjType = documentForm.getType();
			EIMObjectType objType = new EIMObjectType(docObjType.getId(), docObjType.getName(), null);

			//チェックイン実行（DBに登録）
			EIMFormat defaultFormat = FileUtils.getDefaultFormat(sess, objType);
			FileUtils.checkin(sess, documentObj, defaultFormat, documentForm.getName(), targetFile.length());//

			//取得した実ファイルをファイルサーバ上に配置する
			EIMFormat format = FileUtils.getDefaultFormat(sess, objType);
			EIMDirectory dir = format.getDirectory();
			File dstDefaultFile = new File(dir.getPath() + documentObj.getId() + ConfigUtils.getByKey("PDF_EXT"));
			FileUtils.copyFile(targetFile, dstDefaultFile);
			//ちなみに実データを削除する際にはこの実データのファイル名は参照しないので、実データのファイル名は何でもよい

			// 作成したドキュメント自身にステータスが無く、かつ上位フォルダにもステータスが無い場合は、
			// WFなしドキュメントとして、即公開する
			if (documentForm.getStatus() == null && parentForm.getStatus() == null) {

				// 公開ドキュメントとして登録
				EIMFormat publicDocumentFormat = FileUtils.getFormatByName(sess, ConfigUtils.getByKey("FORMAT_NAME_PUBLIC"));
				EIMFile file = FileUtils.getFile(sess, documentObj, FileUtils.getDefaultFormat(sess, objType));
				File orgFile = new File(file.getDirectory().getPath() + FileUtils.getFileName(documentObj, file));
				File dstPublicFile = new File(publicDocumentFormat.getDirectory().getPath() + documentObj.getId() + file.getExt());
				FileUtils.copyFile(orgFile, dstPublicFile);
				FileUtils.checkin(sess, documentObj, publicDocumentFormat, file.getName(), file.getSize());
			}
		} finally {
			if (fis != null) {
				fis.close();
			}
		}
	}

	//==================================
	// Privateメソッド
	//==================================
	/**
	 * ログインユーザがスキャン用表紙作成可能なグループに所属しているかチェックします。
	 * @return チェック結果(true: スキャン用表紙作成可能)
	 * @since Ver6.6
	 */
	private boolean checkCoverMakeAuth() throws Exception {

		boolean hasAuth = false;
		// 紙文書オプションの設定がない場合
		if (!OptionConfData.getInstance().convertPaperToPDFFlg) {
			return hasAuth;
		}

		// セッション取得
		EIMSession sess = EIMThreadContext.getEIMSession();
		// オブジェクトタイプ：グループ 取得
		EIMObjectType groupObjectType = ObjectUtils.getObjectTypeByName(sess, ConfigUtils.getByKey("OBJECT_TYPE_NAME_GROUP"));
		// セッションからユーザ抽出
		EIMUser user = (EIMUser)sess.getAttribute("USER");

		// ユーザが所属しているグループ取得
		List groupList = GroupUtils.getGroupByUser(sess, user);
		// グループ数ループ
		for (int i = 0; i < groupList.size(); i++) {
			EIMGroup group = (EIMGroup)groupList.get(i);
			group = GroupUtils.getGroupById(sess, group.getId());
			while (group != null) {
				List<EIMObject> groupObjectList = ObjectUtils.getObjectListByTypeAndName(sess, groupObjectType, String.valueOf(group.getId()));
				if (groupObjectList != null && groupObjectList.size() == 1) {
					// グループがスキャン用表紙作成可能かどうか
					EIMAttribute coverFlagAttribute = groupObjectList.get(0).getAttribute(ConfigUtils.getByKey("ATTR_NAME_COVER_CREATION_PERMISSION_FLAG_FOR_SCANNING"));
					if (coverFlagAttribute != null) {
						// フラグがオンのグループがある場合、ループを抜ける
						if (coverFlagAttribute.getInt() == 1) {
							hasAuth = true;
							break;
						}
					}
				}
				// 次に親グループをチェック
				group = group.getParent();
			}
			// スキャン用表紙作成可能なグループがある場合、ループを抜ける
			if (hasAuth)
				break;
		}
		return hasAuth;
	}

	/**
	 * 実ファイルなしでドキュメントオブジェクトを作成します。
	 * @param form ドキュメントオブジェクト
	 * @param parentForm 親オブジェクト
	 * @param isAuthCheck 権限チェックを行うかのフラグ(false時チェックしない)
	 * @return チェック結果(true: スキャン用表紙作成可能)
	 * @since Ver6.6
	 */
	private FormDomain createWithoutFile(FormDomain form, FormDomain parentForm, boolean isAuthCheck) throws Exception, EIMApplicationException {

		EIMSession sess = EIMThreadContext.getEIMSession();

		// 作成権限のチェック
		// ワークスペース/フォルダに対する作成権限があるかチェック
		if (isAuthCheck) {
			AccessRoleTypeDomain accessRoleTypedomain = new AccessRoleTypeDomain();
			accessRoleTypedomain.setDefinitionName("CREATE");
			boolean isAccessable = getObjectService().authorized(parentForm, accessRoleTypedomain);
			if (!isAccessable) {
				// 作成権限なし
				throw new EIMApplicationException("EIM.ERROR.LOGIC.DESTINATION.FORM.TYPE.FOLDER.NOCREATEROLE");
			}
		}

		// ワークフロー付きフォルダ下の場合は、編集中以外は作成できない
		if (parentForm.getStatus() != null && parentForm.getStatus().getType().getBase().getId() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {
			throw new EIMApplicationException("EIM.ERROR.LOGIC.NOCREATEROLE");
		}

		// Windows禁止文字をチェック
		AppObjectUtil.checkValidateFName(sess, form.getName());

		// 親オブジェクト(WS / フォルダ)取得
		EIMObject parentObj = ObjectUtils.getObjectById(sess, parentForm.getId());

		//ドキュメントオブジェクト登録
		EIMObjectType objType = new EIMObjectType(form.getType().getId(), null, null);
		EIMObject object = ObjectUtils.createObject(sess, objType, form.getName());

		// リレーション登録
		EIMRelationType relType = RelationUtils.getRelationTypeByName(sess, ConfigUtils.getByKey("RELATION_TYPE_NAME_DOCUMENT"));
		EIMObject childObj = new EIMObject(object.getId(), null, null, 0, false, null, null, null, null, null, null, false, false, null);
		RelationUtils.createRelation(sess, relType, parentObj, childObj,  EIMConstant.DEPU_CHECK_NAME_REV);

		// パスを設定
		String folderPathStr = "/";
		for (AttributeDomain attribute : parentForm.getAttributeList()) {
			if (attribute.getAttributeType().getDefinitionName().equals(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PASS"))) {
				folderPathStr = attribute.getString();
				break;
			}
		}
		String docPathStr = folderPathStr + parentForm.getName() + "/";
		AppObjectUtil.setPath(sess, object, docPathStr);

		// 連続データIDの取得
		String nextValue = AppObjectTypeUtil.getNextValue(objType.getId());
		if (nextValue != null) {
			AppObjectUtil.setAttrForce(sess, object, ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_NUMBER"), nextValue);
		}

		// 上位フォルダからの属性引継ぎ
		// (クライアントから送られる情報は入力可能な属性情報しか送られない為、別途引継属性を設定する。（既存処理）)
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		long[] parentLowAttrIds = AppObjectUtil.getIntAttrs(sess, parentObj, helper.getAttrNameOfToLowAttr());

		if (parentLowAttrIds != null) {
			// 上位からの引継ぎ属性の設定内容に従い、parentObjから属性値をコピー
			// ただし、自身のタイプに該当属性が割り当てられているものに限る
			List<EIMAttributeType> parentLowAttrTypes = new ArrayList<EIMAttributeType>();
			{
				List<Long> parentLowAttrIdsInteger = new ArrayList<Long>(Arrays.asList(ArrayUtils.toObject(parentLowAttrIds)));
				List<?> objectTypes = ObjectAttributeUtils.getAttributeTypeList(sess, objType);
				// 引継ぎ対象を自身のタイプに該当属性が割り当てられているものにフィルタリング
SearchToLowAttr:for (Iterator<Long> i = parentLowAttrIdsInteger.iterator(); i.hasNext();) {
					long attrTypeIdOfParentLowAttrId = ((Long)i.next()).longValue();
					for (Iterator<?> j = objectTypes.iterator(); j.hasNext();) {
						EIMAttributeType attrTypeObj = (EIMAttributeType)j.next();
						if (attrTypeObj.getId() == attrTypeIdOfParentLowAttrId) {
							parentLowAttrTypes.add(attrTypeObj);
							continue SearchToLowAttr;
						}
					}
					i.remove();//自身のタイプに無かったので対象から削除
				}
				parentLowAttrIds = ArrayUtils.toPrimitive((Long[])parentLowAttrIdsInteger.toArray(new Long[parentLowAttrIdsInteger.size()]));
			}
			//「上位からの引継ぎ」属性の値を設定
			ObjectAttributeUtils.setAttribute(sess, object, helper.getAttrTypeOfFromHighAttr()
					, TypeConvertUtils.convertToBuildTypeArray(parentLowAttrIds));

			// 各属性値の引継ぎ
			for (Iterator<EIMAttributeType> i = parentLowAttrTypes.iterator(); i.hasNext();) {
				EIMAttribute attr = parentObj.getAttribute(((EIMAttributeType)i.next()).getDefName());
				if (attr != null) {
					AppObjectUtil.setAttr(sess, object, attr);
				}
			}

			// リスト値表示色オブジェクトの引継ぎ
			DisplayColorUtil.inheritDisplayColor(sess, object, parentLowAttrTypes, parentObj);
		}

		//上位フォルダからのステータス引継ぎ
		if (parentObj.getStatus() != null) {
			WorkFlowUtils.updateObjectStatus(sess, object, parentObj.getStatus());

			//「上位WFフォルダ」属性も登録
			EIMAttribute attrOfHigherWFFolder = parentObj.getAttribute(helper.getAttrNameDocumentHigherWFFolder());
			// WF付フォルダ直下の場合
			if (attrOfHigherWFFolder == null) {
				ObjectAttributeUtils.setAttribute(sess, object, helper.getAttrTypeOfHigherWFFolder(), parentObj.getId());

			//「WF付フォルダ下のフォルダ」の下の場合
			} else {
				AppObjectUtil.setAttr(sess, object, attrOfHigherWFFolder);
			}
		}

		// セキュリティを設定
		EIMSecurity sec = parentObj.getSecurity();
		if (sec != null) {
			SecurityUtils.setSecurity(sess, object, sec);
		}

		// 属性リストの作成
		FormDomain document = form.clone();
		document.setId(object.getId());

		// オブジェクトタイプを再セット
		// オブジェクト生成時にオブジェクトタイプをコピーする（Framework）が、型が異なるのでコピーされないため
		document.setType(form.getType());

		// アクセス履歴登録
		AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.INITIALREGIST");

		// SearchFramework 検索FW更新通知 対象：ドキュメント + 親フォルダ・親ワークスペース
		AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_CREATE_DOCUMENT");
		AppUpdateNoticeUtils.updateNoticeInsertParent(sess, parentObj, "SEARCHFW_CREATE_DOCUMENT_PARENT_FOLDER", "SEARCHFW_CREATE_DOCUMENT_PARENT_WORKSPACE", null);

		// 操作履歴登録
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.CREATE_DOCUMENT,
				EIMConstant.TARGET_CREATE, EIMConstant.OBJECT, object,
				null, null, null, docPathStr);

		return this.create(document);
	}

	//==================================
	// Getter/Setter
	//==================================
	/**
	 * ファイルIOサービスを取得します。
	 * @return ファイルIOサービス
	 */
	public FileIoService getFileIoService() {
		return fileIoService;
	}

	/**
	 * ファイルIOサービスを設定します。
	 * @param fileIoService ファイルIOサービス
	 */
	public void setFileIoService(FileIoService fileIoService) {
		this.fileIoService = fileIoService;
	}

	/**
	 * QRコードサービスを取得します。
	 * @return QRコードサービス
	 */
	public QRCodeService getQrCodeService() {
		return qrCodeService;
	}

	/**
	 * QRコードサービスを設定します。
	 * @param qrCodeService QRコードサービス
	 */
	public void setQrCodeService(QRCodeService qrCodeService) {
		this.qrCodeService = qrCodeService;
	}

	/**
	 * PDFサービスを取得します。
	 * @return PDFサービス
	 */
	public PDFService getPdfService() {
		return pdfService;
	}

	/**
	 * PDFサービスを設定します。
	 * @param pdfService PDFサービス
	 */
	public void setPdfService(PDFService pdfService) {
		this.pdfService = pdfService;
	}
}
