package jp.co.ctc_g.eim.admin.presentation.web.controller;

import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.text.SimpleDateFormat;
import java.util.Date;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

import jp.co.ctc_g.eim.admin.business.domain.ExportDomain;
import jp.co.ctc_g.eim.admin.business.service.AdminUserService;
import jp.co.ctc_g.eim.framework2.business.file.FileAccessor;
import jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;
import jp.co.ctc_g.eim.framework2.integration.file.impl.FileAccessorImpl;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.poi.ss.usermodel.Workbook;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.servlet.ModelAndView;

import common.util.AppMessageUtils;
import common.util.CsvOutputUtil;

/**
 * ファイル転送コントロールクラス
 * Spring MVC のコントローラクラスです。
 */
@Controller
@RequestMapping(value = "/eim/admin/file_io")
public class AdminFileTransferController {
	
	/** ログ */
	private static Log log = LogFactory.getLog(AdminFileTransferController.class);
	/** 最大ファイルサイズ */
	private long maxFileSize;
	/** 正常終了時に遷移するビュー */
	private String resultView;

	
	/**
	 * 実ファイルを作成します。<br>
	 * '@RequestMapping の value 属性に指定された URLのリクエストをクライアントより受信した場合にコールされます。<br>
	 * 実ファイルの作成処理は fileTransferService に処理委譲します。<br>
	 * 作成した実ファイルへのパス及び正常終了時用のビューを ModelAndView インスタンス（ビュー及びビューへ引き継ぐ情報）<br>
	 * に設定し戻り値として返します。<br>
	 *
	 * @param file MultipartFileオブジェクト<br>
	 * <p style="padding-left:4em">作成するファイルデータを指定します。<br>
	 * @return ModelAndViewオブジェクト
	 * <p style="padding-left:4em">ModelAndViewとして、正常終了時に遷移するビューと作成された一時的な実ファイルのパスを返します。<br>
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 */
	@RequestMapping(value = "/create_file")
	public ModelAndView createFile(@RequestParam("file") MultipartFile file) throws Exception {

		if(file == null){
			//引数fileとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.FILE.FILE.VALUE.ILLEGAL");
		}

		if(file.getSize() > maxFileSize)
		{
			//作成できるファイル容量を超えています。
			throw new EIMException("EIM.ERROR.LOGIC.FILE.CREATE.SIZE.OVER");
		}
		
		//実ファイル作成（JVM 上で一意）
		File tempFile = new File(ConfigUtils.getByKey("TEMP_UPLOAD_DIR"));
		File outputFile = File.createTempFile(file.getOriginalFilename(), null, tempFile);

		//ファイル書き込み
		OutputStream outputStream = new FileOutputStream(outputFile);
		FileAccessor fileAccessor = (FileAccessor)ApplicationContextLoader.getApplicationContext().getBean("fileAccessor2");
		FileAccessorImpl fileAccessorImpl = (FileAccessorImpl)fileAccessor;

		fileAccessorImpl.storeFile(file.getInputStream(), outputStream);

		//一時ファイルパス
		String tempFilePath = outputFile.getAbsolutePath();
		
		//ModelAndViewへ返却値をセット
		ModelAndView mav = new ModelAndView();
		mav.setViewName(resultView);
		mav.addObject("result", tempFilePath);
		return mav;
	}
	
	/**
	 * ワークブック(xlsx)をダウンロードします。
	 * @param request HttpServletRequest
	 * @param response HttpServletResponse
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 */
	@RequestMapping(value = "/export_user")
	public void xlsxDownload(HttpServletRequest request, HttpServletResponse response) throws Exception {
		
		try {
			AdminUserService adminUserService = (AdminUserService) ApplicationContextLoader.getApplicationContext().getBean("adminUserService");
			HttpSession session = request.getSession(false);
			
			// 現在日付取得
			Date date = new Date();
			SimpleDateFormat forFileName = new SimpleDateFormat("yyyyMMddHHmmss");
			SimpleDateFormat forXLSX = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
			String exportDateForFileName = forFileName.format(date);
			String exportDateForXLSX = forXLSX.format(date);
			
			// ファイル名作成
			String filename = getFileName(exportDateForFileName);
			
			// ExportDomain設定
			ExportDomain exportDomain = new ExportDomain();
			exportDomain.setSearchUserCode(request.getParameter("searchUserCode"));
			exportDomain.setSearchUserName(request.getParameter("searchUserName"));
			exportDomain.setSearchUserMail(request.getParameter("searchUserMail"));
			exportDomain.setBelongingGroupName(request.getParameter("belongingGroupName"));
			exportDomain.setIncludingChildGroup(Boolean.valueOf(request.getParameter("includingChildGroup")));
			exportDomain.setIsNotDisplayInvalidityUser(Integer.parseInt(request.getParameter("isNotDisplayInvalidityUser")));
			exportDomain.setIsNotDisplayValidityUser(Integer.parseInt(request.getParameter("isNotDisplayValidityUser")));
			exportDomain.setAppName(session.getAttribute("ADMIN_APP_ID").toString());
			exportDomain.setDate(exportDateForXLSX);
			exportDomain.setFileName(filename);
			exportDomain.setHostName(request.getServerName());
			
			// Workbook取得
			Workbook workbook = adminUserService.exportUser(exportDomain);
			
			// 出力文字コード
			String charset = CsvOutputUtil.getCharCodeSetting(null);
			
			// ファイル情報ヘッダセット
			response.setHeader("Cache-Control", "max-age=0, must-revalidate");
			response.setContentType("application/octet-stream;charset=" + charset);
			response.setHeader("Content-Disposition", "attachment; filename=\"" + filename + "\"");
			
			// ファイル出力
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			workbook.write(baos);
			byte[] b = baos.toByteArray();
			
			ByteArrayInputStream in = new ByteArrayInputStream(b);
			
			BufferedOutputStream o = new BufferedOutputStream(response.getOutputStream());
			int x;
			try {
				while ((x = in.read()) >= 0) {
					o.write(x);
				}
			} catch (Exception e) {
				in.close();
				o.close();
			} finally {
				if (in != null)
					in.close();
				if (o != null)
					o.close();
			} 
		} catch (Exception e) {
			log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
		}
	}
	
	/**
	 * 	ファイル名を作成します。
	 * @param date エクスポート日時
	 * @return ファイル名
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 */
	private String getFileName(String date) throws Exception {
		// ファイル名ヘッダ取得
		String fileheader = ConfigUtils.getByKey("LBL_USER_EXPORT_FILEHEADER");
		// ファイル名返却
		return fileheader + date + ConfigUtils.getByKey("EXPORT_FILE_TYPE");
	}
	
	/**
	 * 最大ファイルサイズを取得します。
	 * @return maxFileSize 最大ファイルサイズ
	 */
	public long getMaxFileSize() {
		return maxFileSize;
	}
	
	/**
	 * 最大ファイルサイズを設定します。
	 * @param maxFileSize 最大ファイルサイズ
	 */
	public void setMaxFileSize(long maxFileSize) {
		this.maxFileSize = maxFileSize;
	}
	
	/**
	 * ログを取得します。
	 * @return log ログ
	 */
	public static Log getLog() {
		return log;
	}
	
	/**
	 * ログを設定します。
	 * @param  log ログ
	 */
	public static void setLog(Log log) {
		AdminFileTransferController.log = log;
	}
	
	/**
	 * 正常終了時に遷移するビューを取得します。
	 * @return 正常終了時に遷移するビュー
	 */
	public String getResultView() {
		return resultView;
	}
	
	/**
	 * 正常終了時に遷移するビューを設定します。
	 * @param resultView 正常終了時に遷移するビュー
	 */
	public void setResultView(String resultView) {
		this.resultView = resultView;
	}
}
