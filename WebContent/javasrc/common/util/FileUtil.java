package common.util;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.lang3.StringUtils;

public class FileUtil
{
	/**
	 * 指定したファイル・フォルダを削除する（フォルダの場合は再帰的に削除する）
	 * @param root トップのディレクトリ
	 * @return 削除処理成功／失敗
	 */
	public static boolean clean(File root) 
	{
		if(root == null || root.exists() == false) {
			return false;
		}
		
		if(root.isFile()) {
			if(root.delete() == false) {
				return false;
			}
		}
		else {
			//ディレクトリの場合は再帰処理
			File[] list = root.listFiles();
			for(int ii = 0; ii < list.length ;ii++ ) {
				if(clean(list[ii]) == false) {
					return false;
				}
			}
			if(root.delete() == false) {
				return false;
			}
		}
		return true;
	}
	
	/**
	 * 配下のリストをパスでソートして取得する
	 */
	public static File[] sortedListFiles(File root)
	{
		File[] ret = root.listFiles();
		if(ret == null) {
			return new File[0];
		}
		
		List list = Arrays.asList(ret);
		try {
			list = AppObjectUtil.getStrSortedList(list, "getName", true);
		}
		catch(Exception e) {
			e.printStackTrace();
		}
		
		return (File[])list.toArray(new File[0]);
	}
	
	/**
	 * パスから、オブジェクトの名称を取得する
	 */
	public static String getNameFromPath(String path)
	{
		String targetStr = path;
		
		if (StringUtils.isEmpty(targetStr)){
			return targetStr;
		}
		
		if (targetStr.charAt(targetStr.length() -1) == '/' ){
			targetStr = targetStr.substring(0, targetStr.length() - 1);
			
		}
		
		
		int idx = targetStr.lastIndexOf("/");
		
		if (idx != -1){
			return targetStr.substring(idx + 1);
		}
		else {
			return targetStr;
		}
	}
	
	/**
	 * パスから、親パスを取得する
	 */
	public static String getParentPathFromPath(String path)
	{
		String targetStr = path;
		
		if (StringUtils.isEmpty(targetStr)){
			return targetStr;
		}
		
		if (targetStr.charAt(targetStr.length() -1) == '/' ){
			targetStr = targetStr.substring(0, targetStr.length() - 1);
			
		}
		
		int idx = targetStr.lastIndexOf("/");
		
		if (idx != -1){
			
			return targetStr.substring(0, idx);
		}
		else {
			return targetStr;
		}
	}

	/**
	 * ディレクトリ使用量です。
	 */
	public static class DirectoryUsage {
		/** ファイル数 */
		public int fileCount;
		/** ファイルサイズ合計 */
		public long fileSize;
	}

	/**
	 * ディレクトリ使用量を取得します。
	 * @param directoryPathName ディレクトリパス名
	 * @return ディレクトリ使用量
	 * @throws IOException
	 */
	public static DirectoryUsage getDirectoryUsage(String directoryPathName) throws IOException {

		DirectoryUsage directoryUsage = new DirectoryUsage();

		// ディレクトリパスを取得
		Path directoryPath = Paths.get(directoryPathName);
		if (!Files.exists(directoryPath) || !Files.isDirectory(directoryPath)) {
			// 該当のディレクトリが存在しない場合は空のまま返却
			return directoryUsage;
		}

		String osName = System.getProperty("os.name").toLowerCase();

		// ファイル一覧を取得して件数とファイルサイズ合計をカウントアップ
		Files.list(directoryPath).filter(file -> !Files.isDirectory(file)).forEach(file -> {
			// ファイル数
			directoryUsage.fileCount ++;
			// ファイルサイズ合計
			try {
				if (Files.isSymbolicLink(file)) {
					// シンボリックリンクの場合
					if(osName.startsWith("windows")){
						// Windowsの場合、シンボリックリンクは0バイトのため何もしない
					} else {
						// Linuxの場合、シンボリックリンクはリンク元のパスの文字数がサイズとなる
						directoryUsage.fileSize += Files.readSymbolicLink(file).toString().length();
					}
				} else {
					// 通常ファイルの場合
					directoryUsage.fileSize += Files.size(file);
				}
			} catch (IOException e) {
				// 例外は無視する
			}
		});

		return directoryUsage;
	}
}
