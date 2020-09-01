begin;

delete from users;
delete from images;
delete from mime_types;
delete from news;

insert into news (title, body, "date") values
       ('GHC 14.0 has released', 'See ghc.haskell.org for details.', '2020-07-27')
       , ('New profunctors package has been available', 'See hackage.org', '2021-02-02');

insert into mime_types (value) values
       ('image/png')
       , ('image/jpeg');

insert into images (content, mime_type_id) values
       ('\x89504e470d0a1a0a0000000d494844520000001c0000001408060000009e5e5df9000000017352474200aece1ce9000000097048597300000b1300000b1301009a9c180000015969545874584d4c3a636f6d2e61646f62652e786d7000000000003c783a786d706d65746120786d6c6e733a783d2261646f62653a6e733a6d6574612f2220783a786d70746b3d22584d5020436f726520352e342e30223e0a2020203c7264663a52444620786d6c6e733a7264663d22687474703a2f2f7777772e77332e6f72672f313939392f30322f32322d7264662d73796e7461782d6e7323223e0a2020202020203c7264663a4465736372697074696f6e207264663a61626f75743d22220a202020202020202020202020786d6c6e733a746966663d22687474703a2f2f6e732e61646f62652e636f6d2f746966662f312e302f223e0a2020202020202020203c746966663a4f7269656e746174696f6e3e313c2f746966663a4f7269656e746174696f6e3e0a2020202020203c2f7264663a4465736372697074696f6e3e0a2020203c2f7264663a5244463e0a3c2f783a786d706d6574613e0a4cc227590000055449444154480d95564d4c5c5514feee7d6fde30c3ff4f690b0aad7f9b26edc22e6b0b319ab4893136321663222e2814a55a63cac20da95dd8a84d44a505ab290b1b044a6cdc3426328d36fe24926eaa0b114a0191e11f0666989937effadd3b408875d19ec99bf7b873eff9cef9cef9ce43743f82276c0faf0b0f0b0a10d2424122862bb511fcaadec0bb10c885c21aaf1c3eff293e452768aa1552b4c2d3cf0f62769ec4dd781a4f6ff761cfb40b14096021073588a00c162172d08255bab479d13d835820682fe6e0e34ae241c0f45e79e42f24d20aa1888b34b1309fc67221b0b3af029de2639cc5226e1218cc71451fa07da4dec476f10912ccd2c92cddffb76c2568680c7f90ced33974ccbb6fd645aac0417d1b70088fe2799307735509c410c4c348e39c8620a5494dadea81a5c2b0955abfebe78d4baf6daceb32f40056087441ebab44385fa26ad943d461ed5c0bb32f0c639b3a894664e102e1628c28003fa95e438d68471f016d02b318f7679a45f4ec8113fa1dc96bbbb1d7f5f08b4f20905258c9b790b39842d78be3a8739bf08315c453045a64f50a08f10f33df2b3a31abbe0f94c37162703d8b0c90a4ffb1849208aeac1a40dd9d9d4fc26e1844ea6a054e675b38b7ea316b0595eb833d328c671b2e61d0bb853976b16e9e653ace431c5fb196afa81f835f42e2287f99e4992ce6ffdfeed541e413e64303a8e30957c1aebe91a1e66a256e644b1c226882d4fa5312334747504a6a8f93ce0e02e9ee240f6c9a511cc6c5a2db185d1b879feed6e8dbda746b9ac27ce5712d8236a9c1b469b09e87e882c6e44f122ca5c192d420a5b3adb71c97451b3a550c6166e7e7b6243fec675c11c5f313f0642d580786c230f57dcbb5c67dfa125e724b285ca069d0d004e2a4b6d92fd1c673e693eb871c19424dc3790ca8610c8b4c1d571962369df590da9754387000b68cc29209f6a9456845ce0435ace0f16f89e9cd0c33705bbea5519f5e60a8103a3216c665e5c036d763409b437a3de61b528d0889eaf84daca608910e320845d1d8148d34609406f78a8d83e6747817b2aa4711efdf8d7dcac3fb5e267fad49ff5c14979b806f1a6f61406673fec40d490eddea2ca85e5c6464dfa2c43e88420a6666bd6f744afa51d770c13bbf99a1699a51e3042a8db61cc96661fd48ab7fc9c34c680aaf71c2d48b6c5473579230ec5f9224787150b0ae8554eb17627fec0226d57594ac574b23e8ced6a909290d202313b9511ea5f557a08513e760d43371590e374f8de065750945cae5e0d6c5d75959fcb8b8abddf09266dee6a3561dc67338e0d462d2bb430f11eebfc3461ac38c5ae2be6903b221fc752a7f667706d89d19e1bb14fe18ea3c0a5f6c08dfa1f093d49c1ffbe8ac8bf723bcc7987390414430c881f013a6d5b56019cafdab887bec0845ace5f83da38d12a88a2a447d0ab9691b331c6da5ea2d9c607bb46f8e36875925512b3e4337b5f938f3bdcde8753de3947d80047fcdc17f8c6bf7d8660dfb2bf14e814530ad224e8b205f3eb3c338a63e4029a36ea72b6d8239083aec3260cd947a1b8608748a4019d3945b94c829bcaa175407821cee8efa0d3e3de4a51edcd4de63ace399a5b41967d1221bbe85357c5e0f0c7092749b822bcc93b20081c7e9b0c5782f26148defc776d6ea3b931debc400c1a0cea813ecd706f2d2cb57df7ea4440869d95186a065e17a399d71685b3b7d2859e610aa99c071d27516c5ec4acd43368a08a4737c9b7445541db2cceba9c6ac6a800682bafcffa0947bc07315fca59f4f1004246ba65fec1d12652936e7df2e1618af8828e4a796c1b7162d8d25e6f59ea94d82da9318e244e933bfed32832de34cbfe75a31aa9a3957a37886fbe7f91f81aea9a39ab083aa9ccac001ff0227f0394f3544bcdb0000000049454e44ae426082'
       , (select mime_type_id from mime_types where value = 'image/png')
       );

-- Token: "" (empty)
-- Web token consists of a single user id and a comma, e.g. "1,"
insert into users (last_name, avatar_id, created_at, is_admin, token_hash) values
       ('Admin'
       , (select image_id from images limit 1)
       , '2020-08-20'
       , true
       , '\x00e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855'
       );

commit;
