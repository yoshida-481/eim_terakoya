package batch.migration.utils;

import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import batch.migration.domain.ExportCSVDomain;
import eim.util.StringUtils;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;

public class CSVUtils {
	private static Log log = LogFactory.getLog(CSVUtils.class);

	/** 日付警視浮 */
	private static final String DATE_FOMAT = "yyyyMMdd_HHmm";

	/** ダブルクォーテーション文字エスケープ */
	public static final String CSV_ESCDQUOTATION = "\"";

	/** エスケープ済ダブルクォーテーション */
	private static final String ESCAPE_DQ = CSV_ESCDQUOTATION + "\"";

	/**
	 * CSVを出力する
	 *
	 * @param fileName 出力ファイル名
	 * @param 出力CSVのリストデータ
	 */
	public static void outputList(String fileName, List<ExportCSVDomain> exportCSVList) throws Exception{
		log.info("CSV出力処理開始");
		try {
			SimpleDateFormat sdf = new SimpleDateFormat(DATE_FOMAT);
			//現在日時を取得する
			Calendar c = Calendar.getInstance();

			String outputPath = ConfigUtils.getByKey("MIGRATION_FILELIST_PATH") + fileName + "_" + sdf.format(c.getTime()) + ".csv";
			FileWriter outFile = new FileWriter(outputPath);
			log.info("OUTPUT_PATH = " + outputPath);

			BufferedWriter outBuffer = new BufferedWriter(outFile);
			outBuffer.write("ID,名称,パス,サイズ,結果");
			outBuffer.newLine();

			for (int i = 0; i < exportCSVList.size(); i++) {
				ExportCSVDomain exportCSVDomain = (ExportCSVDomain)exportCSVList.get(i);

				String id = escString(Long.toString(exportCSVDomain.getId()));
				String name = escString(exportCSVDomain.getName());
				String path = escString(exportCSVDomain.getPath());
				String size =  escString(Long.toString(exportCSVDomain.getSize()));
				String result = escString(exportCSVDomain.getResult());

				//名称とパスはカンマ区切りの可能性を考えて""で囲む
				outBuffer.write(id + "," + name + "," + path + "," + size + "," + result);
				outBuffer.newLine();
			}

			outBuffer.flush();
			outBuffer.close();

		} catch (FileNotFoundException e) {
			e.printStackTrace();

		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * 文字列をエスケープする。<BR>
	 * エスケープ方法は、Excel2003に従う。
	 * @param str String
	 *
	 * @return String エスケープしてダブルクォーテーションで囲んだ文字列
	 */
	private static String escString(String str) {
		if (str == null) {
			return "\"\"";
		}
		str = StringUtils.convertReturnCede(str);
		str = str.replaceAll("\"", ESCAPE_DQ);

		return "\"" + str + "\"";
	}
}
