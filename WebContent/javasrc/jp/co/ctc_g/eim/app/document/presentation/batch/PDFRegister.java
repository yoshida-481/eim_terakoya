package jp.co.ctc_g.eim.app.document.presentation.batch;

import java.awt.image.BufferedImage;
import java.awt.image.ConvolveOp;
import java.awt.image.Kernel;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.pdfbox.Loader;
import org.apache.pdfbox.cos.COSObject;
import org.apache.pdfbox.io.RandomAccessReadBufferedFile;
import org.apache.pdfbox.pdmodel.DefaultResourceCache;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.graphics.PDXObject;
import org.apache.pdfbox.rendering.ImageType;

import common.util.AppConstant;
import common.util.AppObjectUtil;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMResource;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.MailUtils;
import jp.co.ctc_g.eim.app.document.business.service.CoverDocumentFormService;
import jp.co.ctc_g.eim.app.document.business.service.DocumentFormService;
import jp.co.ctc_g.eim.app.document.business.service.PDFService;
import jp.co.ctc_g.eim.app.document.business.service.QRCodeService;
import jp.co.ctc_g.eim.app.form.business.domain.AttributeTypeLayoutDomain;
import jp.co.ctc_g.eim.app.form.business.domain.FormDomain;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.RelationCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.CodeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RelationDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RelationTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject.SearchConditionBuildHelper;
import jp.co.ctc_g.eim.framework2.business.service.AttributeTypeService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.business.service.RelationService;
import jp.co.ctc_g.eim.framework2.business.service.RelationTypeService;
import jp.co.ctc_g.eim.framework2.business.service.UserService;
import jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework2.common.context.TransactionContext;
import jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum;
import jp.co.ctc_g.eim.framework2.common.exception.EIMApplicationException;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;
import jp.co.ctc_g.eim.framework2.common.util.ResourceUtils;
import jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils;

/**
 * 複合機で取り込んだPDFを読み込み、分割・アップロードするバッチクラスです。
 */
public class PDFRegister {

	/** PDFサービス */
	private static PDFService pdfService = null;
	/** QRコードサービス */
	private static QRCodeService qrService = null;
	/** オブジェクトサービス */
	private static ObjectService objectService = null;
	/** 属性タイプサービス */
	private static AttributeTypeService attributeTypeService = null;
	/** 帳票ドキュメントサービス */
	private static DocumentFormService documentFormService = null;
	/** 帳票ドキュメントサービス */
	private static CoverDocumentFormService coverDocumentFormService = null;
	/** リレーションタイプサービス */
	private static RelationTypeService relationTypeService;
	/** リレーションサービス */
	private static RelationService relationService;
	/** ユーザサービス */
	private static UserService userService;

	/** コンテキストファイル */
	private static final String[] contextFiles = { "applicationContext.xml" };
	/** 同一表紙の同一日登録ファイル数上限 */
	private static int maxCountEachCoverPerDate;
	/** デフォルト言語設定 */
	private static String lang = "";
	/** pdf拡張子 */
	private static String pdfExt = "";
	/** 実行日 */
	public static String scanDateStr = "";
	/** メール改行コード */
	private static String mailNewLineCode;
	/** 日付フォーマット(日まで) */
	private static SimpleDateFormat dateFmtYYYYMMDD = new SimpleDateFormat("yyyyMMdd");
	/** 日付フォーマット(秒まで) */
	private static SimpleDateFormat dateFmtYYYYMMDDHHMISS = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
	/** 表紙ごとの連番最大値MAP<key: オブジェクトID, value: 連番最大値> */
	public static Map<Long, Integer> maxCounterMap = new HashMap<Long, Integer>();
	/** 取込時の作成者 */
	private static UserDomain pdfRegistUser = null;
	/** QRコードサイズ */
	private static int qrCodeSize;
	/** QRコード出力位置_X軸 */
	private static int qrCodePosX;
	/** QRコード出力位置_Y軸 */
	private static int qrCodePosY;

	/** ログ */
	private static Log log = LogFactory.getLog(PDFRegister.class);

	/** 表紙をA4とした場合の長辺のサイズ（ピクセル） */
	private static final int PAPER_LONG_SIDE_SIZE = 792;

	/** QRコード切り出し時の周りの余白 */
	private static final int QR_CODE_MARGIN = 50;

	/** 平滑化フィルタのデータ */
	private static final float[] smoothData = {
		0.11f, 0.11f, 0.11f,
		0.11f, 0.12f, 0.11f,
		0.11f, 0.11f, 0.11f
	};

	/**
	 * PDF読込のメインメソッドです。
	 * @param args PDFファイルのフルパス
	 * @throws Exception
	 */
	public static void main(String[] args) {
		try {
			exec(args);
		} catch (Exception e) {
			log.warn("Failed to execution.", e);
			System.exit(1);
		}
		System.exit(0);
	}

	/**
	 * PDF読込の実行メソッドです。<br>
	 * 受け取ったパスから取得したPDFファイルを、QRコードの記載された表紙ページを元に分割します。<br>
	 * 分割されたPDFファイルは、それぞれ文書としてEIM上にアップロードされます。<br>
	 *
	 * @param args PDFファイルのフルパス
	 * @throws Exception 以下の例外を通知します。
	 *             <p style="padding-left:4em">
	 *             <table width="72%" border="1">
	 *             <tr bgcolor="#EEEEFF">
	 *             <th width="100">エラーコード</th>
	 *             <th>原因、処置</th>
	 *             </tr>
	 *             <tr>
	 *             <td>EIM.ERROR.LOGIC.FIRSTPAGE.ISNOT.COVER</td>
	 *             <td>指定されたPDFファイルの最初のページにQRコードが検出されませんでした。</td>
	 *             </tr>
	 *             <tr>
	 *             <td>EIM.ERROR.LOGIC.NO.BODY.PDF</td>
	 *             <td>読み込んだ表紙ファイルに対応する本体PDFが存在しませんでした。</td>
	 *             </tr>
	 *             <tr>
	 *             <td>EIM.ERROR.LOGIC.FAILED.PDF.CHECKIN</td>
	 *             <td>対象PDFのアップロードに失敗しました。</td>
	 *             </tr>
	 *             <tr>
	 *             <td>EIM.ERROR.LOGIC.FAILED.PDF.DELETE</td>
	 *             <td>読み込み対象PDFの削除に失敗しました。</td>
	 *             </tr>
	 *             </table>
	 * @since Ver 6.6
	 */
	public static void exec(String[] args) throws Exception {

		String targetFileName = args[0];
		// 処理対象ファイル
		File pdfFile = null;
		String scanFileName = "";
		UserDomain errMailSendToUser = null;

		try {
			// ---------------------------------------------
			// 設定取得
			// ---------------------------------------------
			// コンテキストファイル読み込み
			ApplicationContextLoader.init(contextFiles);
			// メール改行コード取得
			mailNewLineCode = ConfigUtils.getByKey("MAIL_NEWLINE");
			// QRコードサイズ
			qrCodeSize = Integer.parseInt(ConfigUtils.getByKey("COVER_DOC_QR_CODE_SCALE"));
			// QRコード出力位置_X軸
			qrCodePosX = Integer.parseInt(ConfigUtils.getByKey("COVER_DOC_QR_CODE_POS_X"));
			// QRコード出力位置_Y軸
			qrCodePosY = Integer.parseInt(ConfigUtils.getByKey("COVER_DOC_QR_CODE_POS_Y"));
			// PDF拡張子
			pdfExt = ConfigUtils.getByKey("PDF_EXT");
			// スキャン取り込み時の作成者ユーザID
			String scanCreateUserCode = ConfigUtils.getByKey("PDF_AUTO_REGIST_CREATE_USERID_FOR_SCANNING");
			// スキャン取り込みエラー発生時のメール送信先ユーザ
			String errMailSendToUserCode = ConfigUtils.getByKey("PDF_AUTO_REGIST_DEFAULT_MAIL_TO");

			// lang取得
			lang = ConfigUtils.getByKey("MESSAGELANG");
			if (StringUtils.isBlank(lang)) {
				lang = AppConstant.LANG_VALUE_JA;
			}
			maxCountEachCoverPerDate = (int) Math.pow(10, AppConstant.SCAN_SEQ_MAX_SCALE);


			pdfFile = new File(targetFileName);
			// PDF以外の場合、処理を行わずに本バッチを終了
			if (!targetFileName.endsWith(pdfExt)) {
				return;
			}

			// 開始ログ
			log.info(EIMResource.getMessage("EIM.INFO.LOGIC.PDFREGISTER.START"));

			// 引数より処理対象ファイル名取得
			scanFileName = pdfFile.getName();

			// ファイル名ログ出力
			Object[] msgArgs = new Object[]{scanFileName};
			log.info(EIMResource.getMessage("EIM.INFO.LOGIC.PDFREGISTER.FILENAME", msgArgs));

			// ---------------------------------------------
			// サービス設定
			// ---------------------------------------------
			pdfService = (PDFService) ApplicationContextLoader.getApplicationContext().getBean("pdfService");
			qrService = (QRCodeService) ApplicationContextLoader.getApplicationContext().getBean("qrCodeService");
			objectService = (ObjectService) ApplicationContextLoader.getApplicationContext().getBean("objectServiceWithoutAttribute");
			attributeTypeService = (AttributeTypeService) ApplicationContextLoader.getApplicationContext().getBean("attributeTypeService2");
			documentFormService = (DocumentFormService) ApplicationContextLoader.getApplicationContext().getBean("documentFormService");
			coverDocumentFormService = (CoverDocumentFormService) ApplicationContextLoader.getApplicationContext().getBean("coverDocumentFormService");
			relationService = (RelationService) ApplicationContextLoader.getApplicationContext().getBean("relationService2");
			relationTypeService = (RelationTypeService) ApplicationContextLoader.getApplicationContext().getBean("relationTypeService2");
			userService = (UserService) ApplicationContextLoader.getApplicationContext().getBean("userService2");

			// -----------------------------------
			// トランザクション生成
			// -----------------------------------
			createTransaction();

			// -----------------------------------
			// PDF登録処理
			// -----------------------------------
			// 設定ファイルに指定された取り込みPDFの作成者とするユーザを取得
			if (!StringUtils.isBlank(scanCreateUserCode)) {
				pdfRegistUser = userService.getByCode(scanCreateUserCode);
			}

			// エラーメール送信先ユーザを取得
			if (!StringUtils.isBlank(errMailSendToUserCode)) {
				errMailSendToUser = userService.getByCode(errMailSendToUserCode);
			}
			// エラーメール送信先ユーザが設定されていない場合、終了
			if (errMailSendToUser == null) {
				log.error(EIMResource.getMessage("EIM.ERROR.LOGIC.ERRMAIL.SENDTO.USER.NOTFOUND"));
				return;

			} else if (StringUtils.isBlank(errMailSendToUser.getMail())) {
				log.error(EIMResource.getMessage("EIM.ERROR.LOGIC.ERRMAIL.SENDTO.USER.ADDRESS.UNSET"));
				return;
			}


			maxCounterMap = new HashMap<Long, Integer>();
			scanDateStr = dateFmtYYYYMMDD.format(new Date());
			doPdfRegister(pdfFile);

			// コミット
			EIMThreadContext.getTransactionContext().getDBConnection().commit();

			// 終了ログ
			log.info(EIMResource.getMessage("EIM.INFO.LOGIC.PDFREGISTER.END"));

		} catch (Error e) {
			// エラー情報出力
			e.printStackTrace();

			try {
				// ロールバック
				EIMThreadContext.getTransactionContext().getDBConnection().rollback();
			} catch (Exception ex) {
			}

			try {
				// エラーメール送信(送信先が取得出来ていない状態の場合、処理を行わない)
				if (errMailSendToUser != null) {
					sendErrorMail(scanFileName, e.getMessage(), errMailSendToUser);
				}
			} catch (Exception ex) {
				ex.printStackTrace();
			}

		} catch (Exception e) {
			// エラー情報出力
			e.printStackTrace();

			try {
				// ロールバック
				EIMThreadContext.getTransactionContext().getDBConnection().rollback();
			} catch (Exception ex) {
			}

			try {
				// エラーメール送信
				if (errMailSendToUser != null) {
					sendErrorMail(scanFileName, e.getMessage(), errMailSendToUser);
				}
			} catch (Exception ex) {
				ex.printStackTrace();
			}

		} finally {

			if (pdfFile != null) {
				try {
					pdfFile.delete();
					// PDFファイル削除失敗時エラー
					if (pdfFile.exists()) {
						log.error(EIMResource.getMessage("EIM.ERROR.LOGIC.FAILED.PDF.DELETE"));
						throw new EIMException("EIM.ERROR.LOGIC.FAILED.PDF.DELETE");
					}
				} catch (Exception e) {
					// エラー情報出力
					e.printStackTrace();
				}
			}

			// DBクローズ
			if (EIMThreadContext.getTransactionContext() != null) {
				EIMThreadContext.getTransactionContext().getDBConnection().close();
			}
		}
	}

	/**
	 * PDFファイルの登録を実行する
	 *
	 * @throws Exception 例外処理
	 */
	private static void doPdfRegister(File pdfFile) throws Exception {
		PDDocument pdfdoc = null;
		FileInputStream pdffis = null;
		try {
			pdffis = new FileInputStream(pdfFile);
			try {
				pdfdoc = Loader.loadPDF(new RandomAccessReadBufferedFile(pdfFile));
				pdfdoc.setResourceCache(new LocalResourceCache());
			} catch(IOException ioe) {
				log.error(EIMResource.getMessage("EIM.ERROR.LOGIC.CANT.READ.PDF", new Object[]{pdfFile.getName()}));
				throw ioe;
			}
			long coverObjectId = -1;
			int startPage = 2;												// PDF本体開始ページ番号保持用(インデックスでない)
			int allPageNum = pdfdoc.getNumberOfPages();
			String workPath = ConfigUtils.getByKey("PDF_AUTO_REGIST_WORK") + pdfFile.getName();

			FormDomain dividedForm = null;

			for (int i = 0; i < allPageNum; i++) {

				String thisPageCoverQRCode = getCoverQRCode(pdfdoc, i);
				boolean isCoverPage = (thisPageCoverQRCode != null);

				// 表紙の場合
				if (isCoverPage) {

					// 最終ページが表紙の場合
					if (i == allPageNum - 1) {
						log.error(EIMResource.getMessage("EIM.ERROR.LOGIC.NO.BODY.PDF"));
						throw new EIMException("EIM.ERROR.LOGIC.NO.BODY.PDF");
					}

					// 1ページ目の場合
					if (i == 0) {
						coverObjectId = Long.valueOf(thisPageCoverQRCode);
						continue;
					}

					// 本体ページがない場合(表紙が連続した場合)
					if (startPage == i + 1) {
						log.error(EIMResource.getMessage("EIM.ERROR.LOGIC.NO.BODY.PDF"));
						throw new EIMException("EIM.ERROR.LOGIC.NO.BODY.PDF");
					}

					// 本体を登録
					File dividedFile = pdfService.split(pdfdoc, workPath, startPage, i);
					try {
						dividedForm = create(coverObjectId, dividedFile);

						// 登録時ログ出力
						Object[] args = new Object[]{dividedForm.getName(), String.valueOf(dividedForm.getId())};
						log.info(EIMResource.getMessage("EIM.INFO.LOGIC.PDFREGISTER.OBJID", args));

					} finally {
						if (dividedFile != null) {
							dividedFile.delete();
						}
					}

					coverObjectId = Long.valueOf(thisPageCoverQRCode);
					startPage = i + 2; // 表紙の次のページからスタートする

				// 表紙でない場合
				} else {

					// 1ページ目の場合
					if (i == 0) {
						log.error(EIMResource.getMessage("EIM.ERROR.LOGIC.FIRSTPAGE.ISNOT.COVER"));
						throw new EIMException("EIM.ERROR.LOGIC.FIRSTPAGE.ISNOT.COVER");
					}
					// 最終ページの場合
					if (i == allPageNum - 1) {

						// 本体を登録
						File dividedFile = pdfService.split(pdfdoc, workPath, startPage, i + 1);
						try {
							dividedForm = create(coverObjectId, dividedFile);

							// 登録時ログ出力
							Object[] args = new Object[]{dividedForm.getName(), String.valueOf(dividedForm.getId())};
							log.info(EIMResource.getMessage("EIM.INFO.LOGIC.PDFREGISTER.OBJID", args));

						} finally {
							if (dividedFile != null) {
								dividedFile.delete();
							}
						}
					}
				}
			}

		} finally {
			try {
				if (pdfdoc != null) {
					pdfdoc.close();
				}
			} catch (Exception e) {}

			try {
				if (pdffis != null) {
					pdffis.close();
				}
			} catch (Exception e) {}
		}
	}

	/**
	 * 指定ページの表紙QRコードを取得します。表紙QRコードがないページの場合はnullを返却します。<br>
	 *
	 * @param pdfdoc PDFファイル
	 * @param page 対象ページ
	 * @return ヘッダ文字列を除くQRコード
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。<br>
	 */
	private static String getCoverQRCode(PDDocument pdfdoc, int page) throws Exception {

		BufferedImage image = pdfService.toImage(pdfdoc, page, ImageType.GRAY);

		// QRコードのみ切り出す
		BufferedImage qrImage = null;
		if (image.getHeight() > image.getWidth()) {
			// 用紙方向：縦
			int imageLongSideSize = image.getHeight();
			float scale = (float)imageLongSideSize / (float)PAPER_LONG_SIDE_SIZE;

			qrImage = image.getSubimage(
					(int)((qrCodePosX - QR_CODE_MARGIN) * scale),
					(int)((qrCodePosY - QR_CODE_MARGIN) * scale),
					(int)((qrCodeSize + QR_CODE_MARGIN * 2) * scale),
					(int)((qrCodeSize + QR_CODE_MARGIN * 2) * scale));

		} else {
			// 用紙方向：横
			int imageLongSideSize = image.getWidth();
			float scale = (float)imageLongSideSize / (float)PAPER_LONG_SIDE_SIZE;

			qrImage = image.getSubimage(
					(int)((qrCodePosY - QR_CODE_MARGIN) * scale),
					(int)((qrCodePosX - QR_CODE_MARGIN) * scale),
					(int)((qrCodeSize + QR_CODE_MARGIN * 2) * scale),
					(int)((qrCodeSize + QR_CODE_MARGIN * 2) * scale));

		}

		// 平滑化
		BufferedImage smoothQrImage = new BufferedImage(qrImage.getWidth(), qrImage.getHeight(), BufferedImage.TYPE_BYTE_GRAY);
		Kernel smoothKernel = new Kernel(3, 3, smoothData);
		ConvolveOp smoothOp = new ConvolveOp(smoothKernel, ConvolveOp.EDGE_NO_OP, null);
		smoothOp.filter(qrImage, smoothQrImage);

		//イメージを確認したい場合は以下のコメントアウトを外せばファイルに出力されます
//		ImageIO.write(qrImage, "jpeg", new File("sample_" + i + ".jpeg"));
//		ImageIO.write(smoothQrImage, "jpeg", new File("sample_" + i + ".jpeg"));i++;
		String qrCode = qrService.read(smoothQrImage);

		// QRコード読み取った場合の処理
		if (qrCode == null) {
			return null;
		}

		// 無関係なQRコードだった場合
		if (!qrCode.startsWith(AppConstant.QR_CODE_EMBEDDED_HEADER)) {
			return null;
		}
		// プレフィックスと０埋めを除去した値(=objId)を返却
		return qrCode.replace(AppConstant.QR_CODE_EMBEDDED_HEADER, "").replaceFirst("^0+", "");
	}

	/**
	 * 表紙オブジェクトを複製してPDFファイルをチェックインします。<br>
	 *
	 * @param coverObjectId 複製元の表紙オブジェクトのID
	 * @param pdfFile 対象PDFファイル
	 * @return 帳票ドメイン
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。<br>
	 */
	private static FormDomain create(long coverObjectId, File pdfFile) throws Exception {

		EIMUser batchExecUser = EIMThreadContext.getEIMSession().getUser();
		try {
			// 表紙オブジェクト取得
			FormDomain cover = documentFormService.getById(coverObjectId);
			initAttr(cover);

			// 取り込みファイルオブジェクトを表紙オブジェクトを複製し、初期生成
			FormDomain registFile = cover.clone();

			// 取り込みファイル名生成
			registFile.setName(getNameAtMaxCntTargetCoverAndExecuteDate(cover));

			// 取り込みファイル作成者を変更するため、セッションのユーザを一時的に置き換える
			if (pdfRegistUser == null) {
				// 読取登録ユーザが未定義の場合、表紙からセッションを取得する
				EIMThreadContext.getEIMSession().setUser(new EIMUser(cover.getCreationUser().getId(), null, null, null, null, null, 255, 0, null));
			} else {
				// 定義されている場合は該当ユーザをセッション
				EIMThreadContext.getEIMSession().setUser(new EIMUser(pdfRegistUser.getId(), null, null, null, null, null, 255, 0, null));
			}

			// 添付ファイルを引継ぎ対象属性から除く
			List<AttributeTypeLayoutDomain> attrLayoutList = cover.getFormLayout().getObjectTypeLayout().getAttributeLayoutList();
			List<Long> attachedAttributeIds = new ArrayList<Long>();

			for (AttributeTypeLayoutDomain attrLayout : attrLayoutList) {
				String uiControlId = attrLayout.getUiControlId();
				if (uiControlId != null && uiControlId.equals(ConfigUtils.getByKey("UI_CONTROL_ID_ATTACHED_FILE"))) {
					attachedAttributeIds.add(attrLayout.getId());
				}
			}

			List<AttributeDomain> createAttrList = new ArrayList<AttributeDomain>();
			for (AttributeDomain attr : cover.getAttributeList()) {
				// 添付ファイル、スキャン用表紙フラグ以外を登録対象属性として追加する
				if (!attachedAttributeIds.contains(attr.getAttributeType().getId())
					&& !attr.getAttributeType().getName().equals(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_COVER_FOR_SCAN"))) {

					createAttrList.add(attr);
				}
			}
			registFile.setAttributeList(createAttrList);
			initAttr(registFile);

			// 作成者、更新者を設定する
			if (pdfRegistUser != null) {
				registFile.setCreationUser(pdfRegistUser);
				registFile.setModificationUser(pdfRegistUser);
				// 属性：作成者
				AttributeDomain documentCreateUserAttr = registFile.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_CREATE"));
				if (documentCreateUserAttr != null) {
					documentCreateUserAttr.getLongList().set(0, pdfRegistUser.getId());
				}
			}
			// 親オブジェクト(WS / フォルダ)取得
			FormDomain parentForm = new FormDomain(getParent(cover));

			registFile = coverDocumentFormService.createWithoutFileForce(registFile, parentForm);
			coverDocumentFormService.checkInFile(registFile, parentForm, pdfFile);

			return registFile;

		} finally {
			// セッションのユーザを元に戻す
			EIMThreadContext.getEIMSession().setUser(batchExecUser);
		}
	}

	/**
	 * 親オブジェクト(WS / フォルダ)を取得します。
	 *
	 * @param form 表紙オブジェクト
	 * @return 親オブジェクト(WS / フォルダ)
	 * @throws Exception　処理中にエラーが発生した場合、例外を通知します。<br>
	 */
	private static ObjectDomain getParent(FormDomain form) throws Exception {
		// ドキュメントリレーション取得
		RelationTypeDomain relType = relationTypeService.getByDefinitionName(ConfigUtils.getByKey("RELATION_TYPE_NAME_DOCUMENT"));

		// 親ワークスペース、または、フォルダ取得
		RelationCriteria criteria = new RelationCriteria();
		criteria.setChildObjectId(form.getId());
		criteria.setRelationTypeId(relType.getId());
		List<RelationDomain> relList = relationService.getList(criteria);

		if (relList == null || relList.size() == 0) {
			// リレーションが取得できない(=過去版)の場合、処理終了
			log.error(EIMResource.getMessage("EIM.ERROR.LOGIC.PARENT.NOTFOUND"));
			throw new EIMApplicationException("EIM.ERROR.LOGIC.PARENT.NOTFOUND");
		}

		ObjectDomain parentObjectDomain = relList.get(0).getParent();
		ObjectTypeDomain parentObjType = parentObjectDomain.getType();

		EIMObjectType coverObjType = new EIMObjectType(parentObjType.getId(), parentObjType.getName(), null);
		EIMObject coverObj = new EIMObject(parentObjectDomain.getId(), coverObjType, null, 0, false, null, null, null, null, null, null, false, false, null);

		// 親オブジェクトがゴミ箱の下に移動していないかのチェック
		if (AppObjectUtil.isObjectInRecycle(EIMThreadContext.getEIMSession(), coverObj)) {
			Object[] msgArgs = new Object[]{form.getName()};
			log.error(EIMResource.getMessage("EIM.ERROR.LOGIC.COVER.DEELTEED", msgArgs));
			throw new EIMApplicationException("EIM.ERROR.LOGIC.COVER.DEELTEED", msgArgs);
		}
		return parentObjectDomain;
	}

	/**
	 * 属性のnullを空のリストに変更する<br>
	 *
	 * @param form 帳票ドメイン
	 */
	private static void initAttr(FormDomain form) {
		for (AttributeDomain attribute : form.getAttributeList()) {
			if (attribute.getCodeList() == null) {
				attribute.setCodeList(new ArrayList<CodeDomain>());
			}
			if (attribute.getDateList() == null) {
				attribute.setDateList(new ArrayList<Date>());
			}
			if (attribute.getDoubleList() == null) {
				attribute.setDoubleList(new ArrayList<Double>());
			}
			if (attribute.getLongList() == null) {
				attribute.setLongList(new ArrayList<Long>());
			}
			if (attribute.getObjectList() == null) {
				attribute.setObjectList(new ArrayList<ObjectDomain>());
			}
			if (attribute.getStringList() == null) {
				attribute.setStringList(new ArrayList<String>());
			}
			if (attribute.getTextList() == null) {
				attribute.setTextList(new ArrayList<String>());
			}
			if (attribute.getUserList() == null) {
				attribute.setUserList(new ArrayList<UserDomain>());
			}
			if (attribute.getAttributeType().getDefaultDateValueList() == null) {
				attribute.getAttributeType().setDefaultDateValueList(new ArrayList<Date>());
			}
			if (attribute.getAttributeType().getDefaultDoubleValueList() == null) {
				attribute.getAttributeType().setDefaultDoubleValueList(new ArrayList<Double>());
			}
			if (attribute.getAttributeType().getDefaultLongValueList() == null) {
				attribute.getAttributeType().setDefaultLongValueList(new ArrayList<Long>());
			}
			if (attribute.getAttributeType().getDefaultStringValueList() == null) {
				attribute.getAttributeType().setDefaultStringValueList(new ArrayList<String>());
			}
			if (attribute.getAttributeType().getDefaultTextValueList() == null) {
				attribute.getAttributeType().setDefaultTextValueList(new ArrayList<String>());
			}
		}
	}

	/**
	 * トランザクション生成の処理。<br>
	 * <br>
	 * コンソールモードでトランザクションを生成します。<br>
	 * DB接続が成功するまで処理を継続します。<br>
	 *
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 */
	private static void createTransaction() throws Exception {
		// ユーザ生成(systemユーザ)
		EIMUser user = new EIMUser(1, null, null, null, null, null, 255, 0, null);

		// セッション生成
		EIMSession sess = new EIMSession(user, lang);

		// DB接続再試行フラグ
		boolean isRetry = true;

		// トランザクション生成
		TransactionContext tcontext = null;
		while (isRetry) {
			try {
				tcontext = new TransactionContext(ConnectionModeEnum.CONSOLE);
				EIMThreadContext.putTransactionContext(tcontext);
				tcontext.setLangId(sess.getLangId());
				tcontext.setDBConnection(sess.getDBConnection());
				tcontext.setUser(ConvertUtils.toUserDomain(sess.getUser()));

				// DB接続成功時、ループから抜ける
				isRetry = false;

			} catch (Exception e) {
				e.printStackTrace();
				isRetry = true;
				// DB接続失敗時、一定時間待機
				Thread.sleep(Integer.parseInt(ConfigUtils.getByKey("WAIT")));
			}
		}

		// セッションにトランザクションをセットする
		sess.setConnection(tcontext.getDBConnection());
		sess.setConsoleMode(true);

		EIMThreadContext.putEIMSession(sess);
	}

	/**
	 * PDFの読み込み及びアップロードに失敗した際、設定されたユーザに通知します。<br>
	 *
	 * @param scanFileName 処理対象スキャンファイル名
	 * @param errorMessage エラーメッセージ
	 * @param sendToUserDomain 送信先ユーザドメイン
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。<br>
	 */
	private static void sendErrorMail(String scanFileName, String errorMessage, UserDomain sendToUserDomain) throws Exception {
		String userAddr = sendToUserDomain.getMail();
		String langId = sendToUserDomain.getLang();
		if (StringUtils.isBlank(langId)) {
			langId = lang;
		}
		String title = ResourceUtils.getByKeyWithLang(langId, "EIM.MAIL.E.DOCUMENT.UPLOAD.FAILED.TITLE");
		String body = ResourceUtils.getByKeyWithLang(langId, "EIM.MAIL.E.DOCUMENT.UPLOAD.FAILED.MESSAGE")+ mailNewLineCode;
		body += ResourceUtils.getByKeyWithLang(langId, "EIM.MAIL.E.DOCUMENT.UPLOAD.FAILED.TIME",dateFmtYYYYMMDDHHMISS.format(new Date())) + mailNewLineCode;
		body += ResourceUtils.getByKeyWithLang(langId, "EIM.MAIL.E.DOCUMENT.UPLOAD.FAILED.NAME", scanFileName) + mailNewLineCode;
		body += ResourceUtils.getByKeyWithLang(langId, "EIM.MAIL.E.DOCUMENT.UPLOAD.FAILED.DETAIL", errorMessage);
		MailUtils.sendEMail(title, body, userAddr);
	}

	/**
	 * 当日の同一表紙ドキュメントを読み込んだファイルの最大の連番を取得する。<br>
	 *
	 * @param coverDomain 表紙オブジェクト
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 */
	private static String getNameAtMaxCntTargetCoverAndExecuteDate(FormDomain coverDomain) throws Exception {
		// 名称プレフィックスの生成
		String fileNamePrefix = coverDomain.getName().replace(ConfigUtils.getByKey("PDF_AUTO_REGIST_DOC_NAME_PREFIX"), "");
		Pattern pattern = Pattern.compile(pdfExt + "$");
		Matcher matcher = pattern.matcher(fileNamePrefix);
		fileNamePrefix = matcher.replaceAll("");

		fileNamePrefix += AppConstant.DELIMITER_UNDER_SCORE + scanDateStr + AppConstant.DELIMITER_UNDER_SCORE;
		int currentMaxSeq = 0;

		// 対象の表紙オブジェクトを既に一度検索している場合
		if (maxCounterMap.containsKey(coverDomain.getId())) {
			currentMaxSeq = maxCounterMap.get(coverDomain.getId());

		// 対象の表紙オブジェクトが未検索の場合
		} else {
			String attrNameDocumentPath = ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PASS");
			// 属性タイプ: パスを取得
			AttributeTypeDomain pathAttrDomain = attributeTypeService.getByDefinitionName(attrNameDocumentPath);

			String docPath = coverDomain.getAttribute(attrNameDocumentPath).getString();

			// 検索条件設定
			SearchSelectObject searchSelectObject = new SearchSelectObject();
			SearchConditionBuildHelper helper = new SearchConditionBuildHelper();
			searchSelectObject.setCondition(helper.group(helper.opAnd()));
			// 検索条件としてパス属性と名称の一致を指定する
			searchSelectObject.getCondition().addCondition(helper.group(helper.opAnd()).addCondition(helper.eq(helper.opAnd(), pathAttrDomain, docPath)));
			searchSelectObject.getCondition().addCondition(helper.group(helper.opAnd())
							.addCondition(helper.like(helper.opAnd(), SearchSelectObject.PsedoAttributeTypeEnum.NAME, helper.opLike(), (fileNamePrefix + "*" + pdfExt))));

			List<ObjectDomain> documentDomainList = objectService.getList(searchSelectObject, null);
			// 取得結果のNameから連番のMAXを取得
			for (ObjectDomain obj : documentDomainList) {
				// 連番の値を取り出す
				String countStr = obj.getName().replace(fileNamePrefix, "");
				matcher = pattern.matcher(countStr);
				countStr = matcher.replaceAll("");
				if (countStr.length() == AppConstant.SCAN_SEQ_MAX_SCALE) {
					try {
						// 最大かどうか判定
						currentMaxSeq = Math.max(Integer.parseInt(countStr), currentMaxSeq);
					} catch (NumberFormatException e) {
						// 文字を含む場合、最大値の更新は行われない
					}
				}
			}
		}

		int createFileSeq = currentMaxSeq + 1;
		if (createFileSeq >= maxCountEachCoverPerDate) {
			Object[] msgArgs = new Object[]{coverDomain.getName(), String.valueOf(coverDomain.getId())};
			log.error(EIMResource.getMessage("EIM.ERROR.LOGIC.LIMIT.OVER.SAME.COVER.PDF", msgArgs));
			throw new EIMException("EIM.ERROR.LOGIC.LIMIT.OVER.SAME.COVER.PDF", msgArgs);
		}
		maxCounterMap.put(coverDomain.getId(), createFileSeq);

		return fileNamePrefix + StringUtils.leftPad(String.valueOf(createFileSeq), AppConstant.SCAN_SEQ_MAX_SCALE, "0") + pdfExt;
	}

	private static class LocalResourceCache extends DefaultResourceCache {
		@Override
		public void put(COSObject indirect, PDXObject xobject) {
			//super.put(indirect, xobject);
			// 何もしない。不要なキャッシュを防ぐ
		}
	}


}
