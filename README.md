# keter前提でのデプロイ手順

## config/keter.ymlの編集ポイント

------------

```
user-edited: false
```

これを下記のように修正.

```
user-edited: true
```

これをやらないとyesod keterがいつまでたっても実行されない.

------------

```
    exec: ../dist/build/minibas/minibas
```

これを下記のように修正.

```
    exec: ../dist/bin/minibas
```

実行ファイルへのパスを正確に書く必要がある.

上記２点はWebにあまり載っていないようで、ハマリどころになる.

## vboxaddのインストール
vagrantのboxによってはVirtualBox Addonがインストールされていない.  
下記URLのページの記載に従ってインストールする.  

http://zow.hatenablog.com/entry/20150116/1421336024

## .keterファイルをscpで送る

```sh
scp -i <公開鍵ファイルへのパス> ./minibas.keter <リモートホストのユーザ>@<リモートホスト>:/opt/keter/incoming
```

## 環境変数を書く場所

リモート側の設定(/opt/keter/etc/keter-config.yaml)ファイルに書く.

以下のような要領で記述.  
参考）http://qiita.com/erin/items/55e0109e4be6b3257bef

```
env:
  SQLITE_DATABASE: /opt/keter/db/notify.sqlite3
  MAIL_HOST: localhost
  MAIL_PORT: "25"
  MAIL_FROM: from@mydomain.xxx
  MAIL_TO: to@mydomain.xxx
```
