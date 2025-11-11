package jp.co.ctc_g.eim.admin.common.aop.plugin.impl;

import jp.co.ctc_g.eim.framework2.common.aop.plugin.ParameterConverterPlugIn;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;

/**
 * ファイル名を返却するクラスです。
 */
public class ImportFileNameToObjectConverterPlugInImpl implements ParameterConverterPlugIn {
	/**
	 * ファイルパスを受け取り、ファイル名のみを抽出して返却します。
	 * @param source ファイルパス
	 * @return ファイル名
	 */
	public Object convert(Object source) throws Exception {
		String fileName = (String)source;
		int beginIndex = fileName.indexOf(ConfigUtils.getByKey("TEMP_UPLOAD_DIR"));
		beginIndex += ConfigUtils.getByKey("TEMP_UPLOAD_DIR").length();
		int endIndex = fileName.indexOf(ConfigUtils.getByKey("EXPORT_FILE_TYPE"));
		endIndex += ConfigUtils.getByKey("EXPORT_FILE_TYPE").length();
		
		return fileName.substring(beginIndex, endIndex);
	}
}
