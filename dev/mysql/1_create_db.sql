CREATE DATABASE IF NOT EXISTS traume
  CHARACTER SET = 'utf8'
  COLLATE = 'utf8_danish_ci';

USE traume;
CREATE TABLE IF NOT EXISTS `data` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `year` smallint(5) unsigned NOT NULL,
  `var` double(9,3) NOT NULL,
  `denominator` int(10) unsigned NOT NULL DEFAULT 1,
  `ind_id` varchar(63) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

