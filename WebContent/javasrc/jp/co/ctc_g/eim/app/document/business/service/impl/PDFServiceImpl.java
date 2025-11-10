package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;

import javax.imageio.ImageIO;

import org.apache.commons.io.IOUtils;
import org.apache.pdfbox.Loader;
import org.apache.pdfbox.io.RandomAccessReadBufferedFile;
import org.apache.pdfbox.multipdf.Splitter;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.PDPageContentStream;
import org.apache.pdfbox.pdmodel.PDPageContentStream.AppendMode;
import org.apache.pdfbox.pdmodel.font.PDFont;
import org.apache.pdfbox.pdmodel.font.PDType0Font;
import org.apache.pdfbox.pdmodel.graphics.image.PDImageXObject;
import org.apache.pdfbox.rendering.ImageType;
import org.apache.pdfbox.rendering.PDFRenderer;
import org.apache.pdfbox.util.Matrix;

import jp.co.ctc_g.eim.app.document.business.service.PDFService;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;
import jp.co.ctc_g.eim.framework2.common.util.ResourceUtils;


/**
 * @see jp.co.ctc_g.eim.app.document.business.service.PDFService
 */
public class PDFServiceImpl implements PDFService {

	private static final String PNG_EXT = ".png";
	private static final String TEMP_FILE_PREFIX = "pdfImgTemp_";
	private static final String HEADER_DATE_FORMAT = "yyyy/MM/dd HH:mm:ss";

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.PDFService#putImage(java.io.File, java.awt.image.BufferedImage, int, int, int)
	 * @since Ver 6.6
	 */
	public void putImage(File pdfFile, BufferedImage image, int imageScale, int imagePosX, int imagePosY) throws Exception {

		PDDocument doc = null;
		// 一時ファイル
		File tmpImageFile = null;	// 画像ファイル
		File tmpPdfFile = null;		// PDFファイル

		float adjustImageScale = imageScale / image.getHeight();

		PDPageContentStream stream = null;
		FileInputStream fis = null;
		FileOutputStream fos = null;

		try{
			doc = Loader.loadPDF(new RandomAccessReadBufferedFile(pdfFile));
			if (doc.isEncrypted()) {
				doc.setAllSecurityToBeRemoved(true);
			}

			// 一時ファイル作成
			File workDir = new File(ConfigUtils.getByKey("PDF_AUTO_REGIST_WORK"));
			tmpPdfFile = File.createTempFile(TEMP_FILE_PREFIX, ConfigUtils.getByKey("PDF_EXT"), workDir);
			tmpImageFile = File.createTempFile(TEMP_FILE_PREFIX, PNG_EXT, workDir);

			// 引数のQRコードイメージをファイルに出力
			ImageIO.write(image, PNG_EXT.replace(".", ""), tmpImageFile);
			PDImageXObject pdImage = PDImageXObject.createFromFileByContent(tmpImageFile, doc);

			PDPage page = doc.getPage(0);
			stream = new PDPageContentStream(doc, page, AppendMode.APPEND, true);
			stream.drawImage(pdImage, imagePosX, imagePosY, pdImage.getWidth() * adjustImageScale, pdImage.getHeight() * adjustImageScale);
			// 保存前にPDPageContentStreamをクローズする必要あり
			if (stream != null) {
				stream.close();
				// finally句で２度クローズしないようにnullにする
				stream = null;
			}

			// PDFファイルを保存する
			doc.save(tmpPdfFile);

			fis = new FileInputStream(tmpPdfFile);
			fos = new FileOutputStream(pdfFile);
			IOUtils.copy(fis, fos);

		} finally {
			if (stream != null) {
				stream.close();
			}
			if (fis != null) {
				fis.close();
			}
			if (fos != null) {
				fos.close();
			}
			// ファイルクローズ
			if (doc != null) {
				doc.close();
			}
			// 一時ファイルを削除
			if (tmpImageFile != null) {
				tmpImageFile.delete();
			}
			if (tmpPdfFile != null) {
				tmpPdfFile.delete();
			}
		}
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.PDFService#toImage(PDDocument, int, ImageType)
	 * @since Ver 6.6
	 */
	public BufferedImage toImage(PDDocument doc, int pageIndex, ImageType imageType) throws Exception {
		PDFRenderer renderer = new PDFRenderer(doc);
		BufferedImage image = renderer.renderImage(pageIndex, 1, imageType);
		return image;
	}


	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.PDFService#split(PDDocument, String, int, int)
	 * @since Ver 6.6
	 */
	public File split(PDDocument doc, String path, int startPage, int endPage) throws Exception {
		File file = null;
		PDDocument partPDF = null;
		try {
			int divideCounter = 1;
			String pdfExt = ConfigUtils.getByKey("PDF_EXT");

			Splitter splitter = new Splitter();
			splitter.setSplitAtPage(doc.getNumberOfPages());
			splitter.setStartPage(startPage);
			splitter.setEndPage(endPage);
			partPDF = splitter.split(doc).get(0);
			file = new File(path + divideCounter + pdfExt);
			partPDF.save(file);
			partPDF.close();

		} finally {
			// ファイルクローズ
			if (partPDF != null) {
				partPDF.close();
			}
		}
		return file;
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.PDFService#make(String, String, List, int, float, float, float, float, int, String)
	 * @since Ver 6.6
	 */
	public void make(
		String fontFile
		, String filePath
		, List<String> body
		, int fontSize
		, float cdatePosX
		, float cdatePosY
		, float attrPosX
		, float attrPosY
		, int attrLineHeightLarge
		, int attrLineHeight
		, String langId
	) throws Exception {

		PDDocument document = new PDDocument();
		PDPageContentStream contentStream = null;
		try {
			DateFormat sdf = new SimpleDateFormat(HEADER_DATE_FORMAT);
			PDPage page = new PDPage();
			document.addPage(page);
			PDFont font = PDType0Font.load(document, new File(fontFile));

			contentStream = new PDPageContentStream(document, page);
			contentStream.beginText();
			contentStream.setFont(font, fontSize);
			contentStream.setTextMatrix(new Matrix(1, 0, 0, 1, cdatePosX, cdatePosY));
			String createDate = ResourceUtils.getByKeyWithLang(langId, "EIM.E.DOCUMENT.DATE", sdf.format(new Date()));
			contentStream.showText(createDate);
			int height = 0;
			// 属性値表示
			for (int i = 0; i < body.size(); i++) {
				contentStream.setTextMatrix(new Matrix(1, 0, 0, 1, attrPosX, attrPosY - height));
				contentStream.showText(body.get(i));
				if (i == 0) {
					height += attrLineHeightLarge;
				} else {
					height += attrLineHeight;
				}
			}
			contentStream.endText();
			// 保存前にPDPageContentStreamをクローズする必要あり
			if (contentStream != null) {
				contentStream.close();
				// finally句で２度クローズしないようにnullにする
				contentStream = null;
			}

			document.save(filePath);

		} finally {
			if (contentStream != null) {
				contentStream.close();
			}
			if (document != null) {
				document.close();
			}
		}
	}

}
