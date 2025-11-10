package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.awt.image.BufferedImage;
import java.util.concurrent.ConcurrentHashMap;

import com.google.zxing.BarcodeFormat;
import com.google.zxing.BinaryBitmap;
import com.google.zxing.EncodeHintType;
import com.google.zxing.LuminanceSource;
import com.google.zxing.MultiFormatReader;
import com.google.zxing.Reader;
import com.google.zxing.Result;
import com.google.zxing.client.j2se.BufferedImageLuminanceSource;
import com.google.zxing.client.j2se.MatrixToImageWriter;
import com.google.zxing.common.BitMatrix;
import com.google.zxing.common.HybridBinarizer;
import com.google.zxing.qrcode.QRCodeWriter;
import com.google.zxing.qrcode.decoder.ErrorCorrectionLevel;

import common.util.AppConstant;
import jp.co.ctc_g.eim.app.document.business.service.QRCodeService;


/**
 * @see jp.co.ctc_g.eim.app.document.business.service.QRCodeService
 */
public class QRCodeServiceImpl implements QRCodeService {
	
	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.QRCodeService#create(String, int)
	 * @since Ver 6.6 
	 */
	public BufferedImage create(String source, int size) throws Exception {

		ConcurrentHashMap<EncodeHintType, Object> hints = new ConcurrentHashMap<EncodeHintType, Object>();
		hints.put(EncodeHintType.ERROR_CORRECTION, ErrorCorrectionLevel.H);
		hints.put(EncodeHintType.CHARACTER_SET, AppConstant.ENCODE_UTF8);

		QRCodeWriter writer = new QRCodeWriter();
		BitMatrix bitMatrix = writer.encode(source, BarcodeFormat.QR_CODE, size, size, hints);

		return MatrixToImageWriter.toBufferedImage(bitMatrix);
	}
	
	
	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.QRCodeService#read(java.awt.image.BufferedImage)
	 * @since Ver 6.6 
	 */
	public String read(BufferedImage image) {
		try {
			LuminanceSource source = new BufferedImageLuminanceSource(image);
			BinaryBitmap bitmap = new BinaryBitmap(new HybridBinarizer(source));
			Reader reader = new MultiFormatReader();
			Result decodeResult = reader.decode(bitmap);
			return decodeResult.getText();

		} catch  (Exception e) {
			return null;
		}
	}
}
