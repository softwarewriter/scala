create table vessel
(
    imo NVARCHAR(255) NOT NULL,
    name NVARCHAR(255) NOT NULL
)

insert into vessel (imo, name) values ('1', 'Titanic'), ('2', 'Norge'), ('3', 'Eidsvold')
