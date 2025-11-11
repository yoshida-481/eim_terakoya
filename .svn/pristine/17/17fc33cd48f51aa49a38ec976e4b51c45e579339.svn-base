package common.util;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.util.Enumeration;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;

import eim.bo.EIMException;
import eim.net.EIMSession;

public class ZipUtil
{
	/**
	 * Zip解凍する。
	 * @param filename ZIPファイル名
	 * @param outDir 解凍先ディレクトリ
	 * @throws IOException 入出力エラー
	 * @throws EIMException ZIPファイルエラー
	 */
	public static void unZip(EIMSession sess, String filename, String outDir) throws IOException, EIMException
	{
		ZipFile zipFile = null;
		try {
			zipFile = new ZipFile(filename, Charset.forName(AppConstant.ENCODE_MS932));
		}
		catch(IOException e) {
			throw new EIMException(sess, "EIM.ERROR.INPUT.NOTZIPFILE");
		}

		try {
			Enumeration<?> en = zipFile.entries();
			while (en.hasMoreElements())
			{
				ZipEntry entry = (ZipEntry) en.nextElement();

				/** パスにフォルダが含まれている場合は予め作成する **/
				String fName = entry.getName();
				if(fName.lastIndexOf("/") != -1) {
					String path = fName.substring(0, fName.lastIndexOf("/"));
					File folder = new File(outDir + "/" + path);
					folder.mkdirs();
				}

				FileOutputStream out = null;
				try {
					out = new FileOutputStream(outDir + "/" + fName);
				} catch(FileNotFoundException e) {
					//空のディレクトリの場合は何もしない
				}

				InputStream in = zipFile.getInputStream(entry);
				try {
					if (entry.isDirectory()) {
						//空フォルダの場合なにもしない
					}
					else {
						File parent = new File(outDir + "/" + entry.getName()).getParentFile();
						if (parent != null) {
							parent.mkdirs();
						}
						byte[] buf = new byte[1024];
						int size = 0;
						while ((size = in.read(buf)) != -1) {
							out.write(buf, 0, size);
						}
					}
				}
				finally {
					if(out != null) {out.close();}
					if(in != null) {in.close();}
				}
			}
		}
		catch(ZipException e) {
			throw new EIMException(sess, "EIM.ERROR.NOT.DECOMPRESS");
		}
		catch(IOException e) {
			//内側のtry文でスローされた例外を拾うために必要
			throw e;
		}
		finally {
			if(zipFile != null) {
				zipFile.close();
			}
		}
	}
}
