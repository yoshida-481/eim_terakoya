package eim.command.common.util;

public interface EIMCommandResultConstant {
	public static final String NONE					= "";
	public static final String TYPE_INFO		= "INFO";
	public static final String TYPE_WARN		= "WARN";
	public static final String TYPE_ERROR		= "ERROR";
	
	public static final String CODE_AUTH_FAILED								= "EIM-00100";
	public static final String CODE_NO_ESSENTIAL_PARAM						= "EIM-00110";
	public static final String CODE_DUPLICATED_PARAM						= "EIM-00120";
	public static final String CODE_INVALID_OPTION							= "EIM-00130";
	public static final String CODE_OBJECT_NOT_FOUND					= "EIM-00200";
	public static final String CODE_OBJECT_CREATE_NOT_ALLOWED	= "EIM-00300";
	public static final String CODE_NO_READ_AUTH							= "EIM-310";
	public static final String CODE_OBJECT_TYPE_NOT_FOUND			= "EIM-00410";
	public static final String CODE_FORMAT_NOT_FOUND					= "EIM-00420";
	public static final String CODE_OBJECT_NOT_FOLDER					= "EIM-00510";
	public static final String CODE_OBJECT_NOT_DOCUMENT				= "EIM-00520";
	public static final String CODE_OBJECT_NAME_EXISTS				= "EIM-00600";
	public static final String CODE_FILE_NOT_CHECKIN					= "EIM-00700";
	public static final String CODE_FILE_NOT_FOUND						= "EIM-00800";
	public static final String CODE_SYSTEM_ERROR							= "EIM-00900";
	
	public static final String CODE_ALREADY_SIGNED						= "SPX-00110";
	public static final String CODE_SIGNING										= "SPX-00120";
	public static final String CODE_SIGN_EXT_UNAVAILABLE			= "SPX-00130";
	public static final String CODE_SIGN_FAILED								= "SPX-00140";
	public static final String CODE_SIGNED_VERIFY_FAILED			= "SPX-00150";
	public static final String CODE_NOT_SIGNED_DOCUMENT				= "SPX-00160";
	public static final String CODE_FILE_NOT_NLF							= "SPX-00170";
	
	public static final String MSG_AUTH_FAILED								= "認証に失敗しました";
	public static final String MSG_OBJECT_NOT_FOUND						= "指定したオブジェクトは存在しません";
	public static final String MSG_FORMAT_NOT_FOUND						= "指定したフォーマットは存在しません";
	public static final String MSG_OBJECT_NOT_DOCUMENT				= "指定したオブジェクトはドキュメントではありません";
	public static final String MSG_FILE_NOT_CHECKIN						= "指定したフォーマットにファイルがチェックインされていません";
	public static final String MSG_FILE_NOT_FOUND							= "指定したファイルが取得できません";
	public static final String MSG_OBJECT_TYPE_NOT_FOUND			= "指定した文書タイプは存在しません";
	public static final String MSG_OBJECT_NAME_EXISTS					= "指定したファイルは既に存在しています";
	public static final String MSG_OBJECT_NOT_FOLDER					= "指定したオブジェクトはディレクトリではありません";
	public static final String MSG_OBJECT_CREATE_NOT_ALLOWED	= "指定したディレクトリの更新権限がありません";
	public static final String MSG_SYSTEM_ERROR								= "システムエラーが発生しました";
	
	public static final String MSG_ALREADY_SIGNED							= "既に署名・暗号化されています";
	public static final String MSG_SIGNING										= "署名・暗号化を実施中です";
	public static final String MSG_SIGN_EXT_UNAVAILABLE				= "署名・暗号化対象の拡張子ではありません";
	public static final String MSG_SIGNED_VERIFY_FAILED				= "署名・暗号化の検証が失敗しました";
	public static final String MSG_NOT_SIGNED_DOCUMENT				= "署名・暗号化されたデータではありません";
	public static final String MSG_FILE_NOT_NLF								= "指定したデータはNLF形式ではありません";
	
	public static final String MSG_TRANSFER_COMPLETE					= "転送が完了しました";
	public static final String MSG_VERIFY_REQUEST_RECEIVED		= "署名・暗号化処理を受け付けました";
	public static final String MSG_VERIFY_SUCCEEDED						= "署名・暗号化の検証が成功しました";
	public static final String MSG_SIGN_SUCCEEDED							= "署名・暗号化が成功しました";
	public static final String MSG_SIGN_FAILED								= "署名・暗号化に失敗しました";
	public static final String MSG_INVALID_OPTION							= "無効なオプションが指定されました";
	public static final String MSG_DIRECTORY_CREATED					= "ディレクトリを作成しました";
	public static final String MSG_OBJECT_TYPE_DIRECTORY			= "はオブジェクトはディレクトリです";
	public static final String MSG_OBJECT_DELETED							= "オブジェクトを削除しました";
}
